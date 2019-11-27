{-# LANGUAGE TupleSections #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}  -- Persistent :(?
-- {-# OPTIONS_GHC -fdefer-type-errors  #-}
module Main where

import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Data.Text (Text, unpack)
import Data.Text.Encoding
import qualified Data.Text as T
import Data.XML.Types (Event(..), Content(..))
import qualified Data.XML.Types as XT
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Data.Conduit.List as CL
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Base64 (decodeLenient)
import Data.Maybe
import Data.UUID
import System.Random
import Control.Monad.Primitive
import Data.Default
import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Control.Monad
import Data.Conduit.TMChan
import GHC.Conc (atomically, forkIO, STM)
import Conduit
import Control.Concurrent.STM.Map as STC
import Control.Concurrent.STM.TMChan
import Database.Persist
import Database.Persist.Sqlite
import qualified Data.Map as M
import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread.Delay
import Data.Hashable

import Users
import XMLRender
import Concurrency
import XMPP

--------------------------------------------------
-- TLS
--------------------------------------------------

tlsNamespace :: Text
tlsNamespace = "urn:ietf:params:xml:ns:xmpp-tls"

{-
  Need to lock sink until full messages are sent...

  There's a couple problems.

  If I have a stream I don't necessarily know when I have a full piece
  of XML currently. Not sure when response is finished.

  - Must always send full thing.

  Locked right now :|
-}

-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLS :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) => AppData -> m ()
startTLS ad = do
  (sink, chan) <- liftIO $ forkSink (appSink ad)
  startTLS' (appSource ad .| parseBytes def) sink (appSink ad)

startTLS' :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) => ConduitT () Event m () -> ConduitT Element Void m () -> ConduitT BS.ByteString Void m () -> m ()
startTLS' source sink bytesink = runConduit $ do
  -- Use of bytesink is potentially dangerous, but should only be
  -- used before concurrency is an issue
  openStream source bytesink

  -- Send StartTLS feature.
  yield tlsFeatures .| sink

  -- Wait for TLS request from client.
  liftIO $ putStrLn "Awaiting TLS"
  starttls <- source .| awaitStartTls

  -- Tell client to proceed
  liftIO $ putStrLn "Sending TLS proceed"
  yield proceed .| sink
  liftIO $ putStrLn "Closing unencrypted channel."

proceed :: Element
proceed = Element (Name "proceed" (Just tlsNamespace) Nothing) mempty []

tlsFeatures :: Element
tlsFeatures = features [NodeElement tlsFeature]
  where
    tlsFeature = Element tlsName mempty [NodeElement required]
    tlsName    = Name "starttls" (Just tlsNamespace)
                                 Nothing

awaitStartTls :: MonadThrow m => ConduitT Event o m (Maybe Event)
awaitStartTls = awaitName (Name {nameLocalName = "starttls", nameNamespace = Just tlsNamespace, namePrefix = Nothing})

--------------------------------------------------
-- SASL
--------------------------------------------------

saslNamespace :: Text
saslNamespace = "urn:ietf:params:xml:ns:xmpp-sasl"

-- | Get authentication information.
plainAuth
  :: (PrimMonad m, MonadThrow m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
     ConduitT i Event m ()
     -> ConduitT Element o m r
     -> ConduitT i o m (Maybe User)
plainAuth source sink = do
  yield authFeatures .| sink
  auth <- source .| awaitAuth
  case auth of
    Just (user, pass) -> authenticate user pass
    _ -> return Nothing

authenticate
  :: (MonadReader XMPPSettings m, MonadIO m) =>
     Text -> Text -> m (Maybe User)
authenticate user pass = do
  db <- asks xmppDB
  liftIO $ runSqlite db $ do
    maybeUser <- getBy (UniqueName user)
    return $
      case maybeUser of
        Just (Entity _ u) ->
          if userPassword u == pass
          then Just u
          else Nothing
        _ -> Nothing

failure :: [Node] -> Element
failure = Element (Name "failure" (Just saslNamespace) Nothing) mempty

-- TODO on receiving abort we need to reply with this
aborted :: Element
aborted = failure [NodeElement abortElement]
  where abortElement = Element "aborted" mempty []

notAuthorized :: Element
notAuthorized = failure [NodeElement notAuthElement]
  where notAuthElement = Element "not-authorized" mempty []

success :: Element
success = Element (Name "success" (Just saslNamespace) Nothing) mempty []

authFeatures :: Element
authFeatures = features [NodeElement mechanisms]
  where
    mechanisms = Element mechanismsName mempty [NodeElement plain]
    mechanismsName = Name "mechanisms"
                          (Just saslNamespace)
                          Nothing
    plain = Element (Name "mechanism" Nothing Nothing) mempty [NodeContent "PLAIN"]

awaitAuth :: MonadThrow m => ConduitT Event o m (Maybe (Text, Text))
awaitAuth = do
  authStr <- tagIgnoreAttrs (matching (==(Name {nameLocalName = "auth", nameNamespace = Just saslNamespace, namePrefix = Nothing}))) content -- TODO check for mechanism='PLAIN'
  return $ do
    auth <- authStr
    case decodeUtf8 <$> (BS.split 0 . decodeLenient $ encodeUtf8 auth) of
      [_, user, pass] -> return (user, pass)
      _               -> Nothing

--------------------------------------------------
-- Bind
--------------------------------------------------

bindNamespace :: Text
bindNamespace = "urn:ietf:params:xml:ns:xmpp-bind"

bindName :: Name
bindName = Name "bind" (Just bindNamespace) Nothing

bindFeatures :: Element
bindFeatures = features [NodeElement bind]
  where
    bind = Element bindName mempty []

iqShort :: Text -> Text -> [Node] -> Element
iqShort i t = Element "iq" attrs
  where attrs = M.fromList [("id", i), ("type", t)]

bind :: Text -> Element
bind jid = Element bindName mempty [NodeElement bindElement]
  where
    bindElement = Element "jid" mempty [NodeContent jid]

receiveIqBind :: MonadThrow m =>
  (Text -> Text -> ConduitT Event o m r) -> ConduitT Event o m (Maybe r)
receiveIqBind handler =
  tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type") $ uncurry handler

bindHandler :: (MonadThrow m, PrimMonad m, MonadIO m, MonadUnliftIO m) =>
  ChanMap ->
  Text ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
bindHandler cm jid sink i t =
  if t /= "set"
  then error "type =/= set" -- TODO better error handling?
  else tagIgnoreAttrs (matching (==bindName)) doBind
  where
    resourceName = Name "resource" (Just bindNamespace) Nothing
    doBind = do
      resource <- tagIgnoreAttrs (matching (==resourceName)) content

      case resource of
        Nothing -> error "bad resource" -- TODO replace this
        Just resource -> do
          let fullResource = jid <> "/" <> resource
          let iqNodes      = [NodeElement (bind fullResource)]

          liftIO $ putStrLn "Adding channel..."
          createHandledChannel cm jid resource (forwardHandler sink)
          yield (iqShort i "result" iqNodes) .| sink

--------------------------------------------------
-- Queries
--------------------------------------------------

infoNamespace :: Text
infoNamespace = "http://jabber.org/protocol/disco#info"

itemsNamespace :: Text
itemsNamespace = "http://jabber.org/protocol/disco#items"

iqName :: Name
iqName = Name {nameLocalName = "iq", nameNamespace = Just "jabber:client", namePrefix = Nothing}

testIqInfo :: BS.ByteString
testIqInfo = "<iq type='get' from='romeo@montague.net/orchard' to='plays.shakespeare.lit' id='info1'> <query xmlns='http://jabber.org/protocol/disco#info'/> </iq>"

iq :: Text -> Text -> Text -> Text -> [Node] -> Element
iq i t to from = Element "iq" attrs
  where attrs = M.fromList [("id", i), ("type", t), ("to", to), ("from", from)]

receiveIq :: MonadThrow m =>
  (Text -> Text -> Text -> Text -> ConduitT Event o m r) -> ConduitT Event o m (Maybe r)
receiveIq handler =
  tag' (matching (==iqName)) ((,,,) <$>
              requireAttr "id" <*>
              requireAttr "type" <*>
              requireAttr "to" <*>
              requireAttr "from") $ uncurry4 handler
  where
    uncurry4 f (w, x, y, z) = f w x y z

iqHandler :: (MonadThrow m, MonadIO m) =>
  ChanMap ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
iqHandler cm sink i t to from =
  do
    if t /= "get"
      then error "type =/= get" -- TODO
      else do
      query <- tagNoAttr (matching (==infoQueryName)) content

      case query of -- TODO how to refactor
        Nothing -> do
          query <- tagNoAttr (matching (==itemsQueryName)) content
          case query of
            Nothing -> return Nothing
            Just q -> doItems
        Just q -> doInfo
  where
    infoQueryName = Name "query" (Just infoNamespace) Nothing
    itemsQueryName = Name "query" (Just itemsNamespace) Nothing
    doInfo = do
      r <- yield (iq i "result" from to [NodeElement (Element infoQueryName mempty [])]) .| sink
      return $ Just r
    doItems = do
      r <- yield (iq i "result1" from to [NodeElement (Element itemsQueryName mempty [])]) .| sink
      return $ Just r

--------------------------------------------------
-- OTHER STUFF
--------------------------------------------------

streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId =
  yield $ EventBeginElement streamName attrs
  where attrs = [ at "from" from
                , at "version" version
                , at "id" (toText streamId)
                , (Name "lang" (Just "xml") (Just "xml"), [ContentText lang]) ]
        at name content = (Name name Nothing Nothing, [ContentText content])
        streamName = Name "stream"
                          (Just "http://etherx.jabber.org/streams")
                          (Just "stream")

features :: [Node] -> Element
features nodes = Element featureName mempty nodes
  where
    featureName = Name "features"
                       (Just "http://etherx.jabber.org/streams")
                       (Just "stream")

required :: Element
required = Element "required" mempty []

awaitStream :: MonadThrow m => ConduitT Event o m (Maybe Event)
awaitStream = awaitName (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})

awaitName :: MonadThrow m => Name -> ConduitT Event o m (Maybe Event)
awaitName name = do
  element <- await
  case element of
    Just (EventBeginElement n _) ->
      if n == name
        then return element
        else awaitName name
    Nothing -> return Nothing
    _ -> awaitName name

--------------------------------------------------
-- XMPP Stanzas
--------------------------------------------------

data IqStanza = MkIq { iqId   :: Text
                     , iqType :: Text
                     , iqContents :: [Event]  -- Change to a full XML document??
                     }
                deriving (Eq, Show)

receiveMessage handler =
  tag' "{jabber:client}message" ((,,,) <$> requireAttr "to" <*> requireAttr "from" <*> requireAttr "id" <*> requireAttr "type" <* ignoreAttrs) (\(t,f,i,ty) -> handler t f i ty)

messageHandler cm to from i ty = do
  -- Read message contents
  body <- tagIgnoreAttrs "{jabber:client}body" content
  ignoreAnyTreeContent  -- Ignore origin-id, already have information from it.
  ignoreAnyTreeContent  -- Also don't need any information from request tag.
  threadId <- tagIgnoreAttrs "{jabber:client}thread" content

  -- Need to construct message element to send.
  let elem = Element "{jabber:client}message" (M.fromList [("from", from), ("to", to), ("type", ty)])
             [ NodeElement (Element "{jabber:client}body" mempty [NodeContent (fromMaybe (error "bad body") body)])
             , NodeElement (Element "{urn:xmpp:sid:0}origin-id" (M.fromList [("id", i)]) [])
             , NodeElement (Element "{urn:xmpp:receipts}request" mempty [])
             , NodeElement (Element "{jabber:client}thread" mempty [NodeContent (fromMaybe (error "bad body") threadId)])
             ]

  sendToJid cm to elem

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
--
-- If the jid has a resource as well, only send to that specific resource.
sendToJid
  :: MonadIO m =>
     ChanMap -> Text -> Element -> m ()
sendToJid cm to elem =
  case T.splitOn "/" to of
    [jid]           -> sendToJidAll cm jid elem
    [jid, resource] -> sendToResource cm jid resource elem
    (jid:_)         -> sendToJidAll cm jid elem
    _               -> return ()

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
sendToJidAll
  :: MonadIO m =>
     ChanMap -> Text -> Element -> m ()
sendToJidAll cm to elem = do
  channels <- liftIO . atomically $ do
    mm <- STC.lookup to cm
    case mm of
      Nothing      -> return []
      Just (rs, m) -> forM rs $ \r -> do
        maybeChan <- STC.lookup r m
        return (fromMaybe [] (fmap (:[]) maybeChan))
  liftIO $ putStrLn $ "Message to: " ++ show to
  liftIO $ print $ length channels
  writeToAllChannels (Prelude.concat channels) elem

sendToResource :: MonadIO m =>
     ChanMap -> Text -> Text -> Element -> m ()
sendToResource cm jid resource elem = do
  mchan <- liftIO . atomically $ do
    mm <- STC.lookup jid cm
    case mm of
      Nothing     -> return Nothing
      Just (_, m) -> STC.lookup resource m
  case mchan of
    Nothing   -> return ()
    Just chan -> liftIO . atomically $ writeTMChan chan elem

writeToAllChannels
  :: MonadIO m => [TMChan a] -> a -> m ()
writeToAllChannels channels elem =
  forM_ channels $ \chan -> liftIO . atomically $ writeTMChan chan elem

-- | Handler that forwards messages from a channel to a sink.
forwardHandler :: (Show a, MonadIO m, MonadUnliftIO m) => ConduitT a o m r -> TMChan a -> m ()
forwardHandler sink chan = do
  elem <- liftIO $ atomically $ readTMChan chan
  case elem of
    Nothing   -> return ()
    Just elem -> do
--      liftIO . putStrLn $ "Forwarding: " ++ show elem
      runConduit $ yield elem .| void sink .| Conduit.sinkNull
  forwardHandler sink chan

baseIqHandler i t = do
  c <- void takeAnyTreeContent .| consume
  return $ MkIq i t c

userJid :: Text -> User -> Text
userJid fqdn u = userName u <> "@" <> fqdn

-- TODO wrap AppData / configuration in a reader monad?

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m) =>
  ConduitT BS.ByteString o m r -> ConduitT i o m UUID
initiateStream sink = do
    streamId <- liftIO randomIO
    fqdn <- asks fqdn
    liftIO $ putStrLn "Sending stream response..."
    streamRespHeader fqdn "en" streamId .| XR.renderBytes def .| sink
    liftIO $ putStrLn "Done..."
    return streamId

-- | Open a stream.
openStream
  :: (MonadThrow m, PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadIO m) =>
     ConduitT i Event m () ->
     ConduitT BS.ByteString o m r ->  -- ^ Need to use BS because XML renderer doesn't flush.
     ConduitT i o m UUID
openStream source sink = do
  source .| awaitStream
  liftIO $ putStrLn "Got connection stream thing..."
  initiateStream sink

-- | Todo, figure out how to allow for stream restarts at any point.
-- This should be architected more like a state machine.
-- handleClient :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadUnliftIO m) =>
--   AppData -> m ()
--handleClient :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadUnliftIO m) =>
--  Map Text (TMChan Element) -> AppData -> m ()
handleClient cm ad =
  do (sink, chan) <- liftIO $ forkSink (appSink ad)
     handleClient' cm (appSource ad .| parseBytes def) sink (appSink ad)

-- Separated for testing
--handleClient' :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadUnliftIO m) =>
--  Map Text (ConduitT i Void m ()) -> ConduitT () Event m () -> ConduitT Event o m () -> m ()
handleClient'
   :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m, MonadUnliftIO m, Show r) =>
   ChanMap ->
   ConduitT () Event m () ->
   ConduitT Element Void m r ->
   ConduitT BS.ByteString Void m r ->
   m ()
handleClient' cm source sink bytesink = runConduit $ do
  streamid <- openStream source bytesink

  -- Get user and pass
  auth <- plainAuth source sink
  case auth of
    Nothing -> do
      yield notAuthorized .| sink
      liftIO $ putStrLn "Authentication failed."
    Just u  -> do
      yield success .| sink
      liftIO $ print auth

      -- Restart stream and present bind feature.
      openStream source bytesink
      yield bindFeatures .| sink

      fqdn <- asks fqdn
      let jid = userJid fqdn u

      source .| receiveIqBind (bindHandler cm jid sink)

      source .| receiveIq (iqHandler cm sink)
      source .| receiveIq (iqHandler cm sink)
      source .| receiveIq (iqHandler cm sink)

      messageLoop

      liftIO $ print "</stream> ;D"
    where messageLoop = do
            source .| receiveMessage (messageHandler cm)
            messageLoop
--------------------------------------------------
-- Main server
--------------------------------------------------

main :: IO ()
main = do
  putStrLn "Running SQL migrations..."
  runSqlite (xmppDB def) $ runMigration migrateAll
  putStrLn "Starting server..."

  -- Generate channel map.
  cm <- atomically empty
  runReaderT (do port <- asks xmppPort
                 runTCPServerStartTLS (tlsConfig "*" port "cert.pem" "key.pem") (xmpp cm)) def

xmpp :: (PrimMonad m, MonadReader XMPPSettings m, MonadIO m, MonadUnliftIO m, MonadThrow m) =>
  ChanMap -> GeneralApplicationStartTLS m ()
xmpp cm (appData, stls) = do
  startTLS appData
  liftIO $ putStrLn "Starting TLS..."
  stls $ handleClient cm
