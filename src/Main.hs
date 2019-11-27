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

bind :: Text -> Element
bind jid = Element bindName mempty [NodeElement bindElement]
  where
    bindElement = Element "jid" mempty [NodeContent jid]

--------------------------------------------------
-- OTHER STUFF
--------------------------------------------------

-- TODO use IqStanza below?
iq :: Text -> Text -> [Node] -> Element
iq i t = Element "iq" attrs
  where attrs = M.fromList [("id", i), ("type", t)]

streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId =
  yield $ EventBeginElement streamName attrs
  -- How come the client isn't sending us "from"? We need it to send "to"
  where attrs = [ at "from" from
                , at "version" version
                , at "id" (toText streamId)
                , (Name "lang" (Just "xml") (Just "xml"), [ContentText lang])
                ]
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


receiveIq :: MonadThrow m =>
  (Text -> Text -> ConduitT Event o m r) -> ConduitT Event o m (Maybe r)
receiveIq handler =
  tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type" <* ignoreAttrs) $ uncurry handler

receiveMessage handler =
  tag' (matching (==msgname)) ((\t f i -> (t,f,i)) <$> requireAttr "to" <*> requireAttr "from" <*> requireAttr "id" <* ignoreAttrs) (\(t,f,i) -> handler t f i)

{-
<message xmlns="jabber:client" from="test@localhost/gajim.CD9NEZ09" id="6cb35763-f702-49c5-8dc1-2e03a0edc88b" to="foo@localhost" type="chat">
<body>Howdy.</body>
<origin-id xmlns="urn:xmpp:sid:0" id="6cb35763-f702-49c5-8dc1-2e03a0edc88b" />
<request xmlns="urn:xmpp:receipts" />
<thread>qcWrRbPgEZtEWtRxcswgkwhDFpAFZiPR</thread>
</message>
-}


messageHandler cm to from i = do
  liftIO . putStrLn $ "Message to: " ++ show to

  -- Read message contents
  body <- tagIgnoreAttrs "{jabber:client}body" content
  ignoreAnyTreeContent  -- Ignore origin-id, already have information from it.
  ignoreAnyTreeContent
  threadId <- tagIgnoreAttrs "{jabber:client}thread" content

  -- Need to construct message element to send.
  let elem = Element "{jabber:client}message" (M.fromList [("from", from), ("to", to)])
             [ NodeElement (Element "{jabber:client}body" mempty [NodeContent (fromJust body)])
             , NodeElement (Element "{urn:xmpp:sid:0}origin-id" (M.fromList [("id", i)]) [])
             , NodeElement (Element "{urn:xmpp:receipts}request" mempty [])
             , NodeElement (Element "{jabber:client}thread" mempty [NodeContent (fromJust threadId)])
             ]

  sendToJid cm to elem

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
sendToJid
  :: MonadIO m =>
     ChanMap -> Text -> Element -> m ()
sendToJid cm to elem = do
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

skipToEnd :: Monad m => Name -> ConduitT Event Event m ()
skipToEnd name = do
  x <- await
  case x of
    Nothing -> return ()
    (Just e@(EventEndElement n)) -> if n == name then yield e else skipToEnd name
    _ -> skipToEnd name

msgname = Name {nameLocalName = "message", nameNamespace = Just "jabber:client", namePrefix = Nothing}

testiq :: BS.ByteString
testiq = "<iq id=\"5ba62e81-cbbd-45cc-a20a-5abca191b55f\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>gajim.CD9NEZ09</resource></bind></iq>"

testmsg2 :: BS.ByteString
testmsg2 = "<message xmlns=\"jabber:client\" from=\"test\" id=\"0f41469e-55fe-42c8-88a2-997e592ef16d\" to=\"foo@localhost\" type=\"chat\">ahhhhh</message>"

testmsg :: BS.ByteString
testmsg = "<message xmlns=\"jabber:client\" from=\"test@localhost/gajim.CD9NEZ09\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" to=\"foo@localhost\" type=\"chat\"><body>eoaueoa</body><origin-id xmlns=\"urn:xmpp:sid:0\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" /><request xmlns=\"urn:xmpp:receipts\" /><thread>MrwqjWfrzhgjYOPHfuQwOjgWuSTHWIcM</thread></message>"

-- elementFromEvents' :: MonadThrow m => ConduitT EventPos o m (Maybe Document)
-- elementFromEvents' = fmap (fmap documentConvert) fromEvents

-- documentConvert :: XT.Document -> Document
-- documentConvert = undefined

elemConvert :: XT.Element -> Element
elemConvert (XT.Element name attrs nodes)
  = Element name attrs' undefined
  -- TODO: I have no idea if this is remotely right, but I have no choice.
  -- Possibly look up contentsToText in XML stream parse
  where attrs' = M.fromList $
          (\(n, c) -> (n, mconcat $ contentToText <$> c)) <$> attrs

nodeConvert :: XT.Node -> Node
nodeConvert (XT.NodeElement e)     = NodeElement (elemConvert e)
nodeConvert (XT.NodeInstruction i) = NodeInstruction i
nodeConvert (XT.NodeContent c)     = NodeContent (contentToText c)
nodeConvert (XT.NodeComment c)     = NodeComment c

contentToText :: Content -> Text
contentToText (ContentText c)   = c
contentToText (ContentEntity c) = c

writeToAllChannels
  :: MonadIO m => [TMChan a] -> a -> m ()
writeToAllChannels channels elem =
  forM_ channels $ \chan -> liftIO . atomically $ writeTMChan chan elem

bindHandler :: (MonadThrow m, PrimMonad m, MonadIO m, MonadUnliftIO m) =>
  ChanMap ->
  Text ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
bindHandler cm jid sink i _ =
  tagIgnoreAttrs (matching (==bindName)) doBind
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
          yield (iq i "result" iqNodes) .| sink

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

      iqStanza <- source .| receiveIq (bindHandler cm jid sink)
      liftIO $ print iqStanza

      source .| ignoreAnyTreeContent
      source .| ignoreAnyTreeContent
      source .| ignoreAnyTreeContent

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


--------------------------------------------------
-- Internal Server Communication
--------------------------------------------------

eventConduitTest sink = do
  chan <- liftIO newTMChanIO
--  streamId <- liftIO $ randomIO

  let tmSource = sourceTMChan chan
  let tmSink   = sinkTMChan chan
--  let docond = do v <- await; yield (fromJust v); liftIO $ print "test"; liftIO $ print v; docond

  forkIO (runConduit $ tmSource .| renderElements .| sink)
--  forkIO $ chanToSink chan sink
  forkIO (runConduit $ yield testElement .| tmSink)


testElement = Element "message" (M.fromList [("to", "foo"), ("from", "calvin")]) []
testDocument = Document emptyPrologue testElement []
