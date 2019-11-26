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

awaitStartTls :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStartTls = awaitName (Name {nameLocalName = "starttls", nameNamespace = Just tlsNamespace, namePrefix = Nothing})

--------------------------------------------------
-- SASL
--------------------------------------------------

saslNamespace :: Text
saslNamespace = "urn:ietf:params:xml:ns:xmpp-sasl"

-- | Get authentication information.
plainAuth
  :: (PrimMonad m, MonadThrow m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
     ConduitT a1 Event m ()
     -> ConduitT Element c m a2
     -> ConduitT a1 c m (Maybe User)
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

awaitAuth :: MonadThrow m => ConduitT Event a m (Maybe (Text, Text))
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

awaitStream :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStream = awaitName (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})

awaitName :: MonadThrow m => Name -> ConduitT Event a m (Maybe Event)
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


receiveIq :: MonadThrow m => (Text -> Text -> ConduitT Event o m c) -> ConduitT Event o m (Maybe c)
receiveIq handler =
  tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type") $ uncurry handler

bindHandler :: (MonadThrow m, PrimMonad m, MonadIO m) =>
  ChanMap ->
  Text ->
  ConduitT Element o m b ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe b)
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

          r <- yield (iq i "result" iqNodes) .| sink
          createHandledChannel cm fullResource (forwardHandler sink)
          return r

-- | Handler that forwards messages from a channel to a sink.
forwardHandler :: MonadIO m => ConduitT a o m r -> TMChan a -> m ()
forwardHandler sink chan = do
  elem <- liftIO $ atomically $ readTMChan chan
  case elem of
    Nothing   -> return ()
    Just elem -> runConduit $ yield elem .| void sink .| Conduit.sinkNull
  forwardHandler sink chan

baseIqHandler i t = do
  c <- void takeAnyTreeContent .| consume
  return $ MkIq i t c

userJid :: Text -> User -> Text
userJid fqdn u = userName u <> "@" <> fqdn

-- TODO wrap AppData / configuration in a reader monad?

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m) => ConduitT BS.ByteString o m r -> ConduitT i o m UUID
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
     ConduitT a Event m () ->
     ConduitT BS.ByteString c m r ->  -- ^ Need to use BS because XML renderer doesn't flush.
     ConduitT a c m UUID
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
-- handleClient'
--    :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m,
--        MonadUnliftIO m, Show b) =>
--    Map Text (TMChan Element) ->
--    ConduitT () Event m () -> ConduitT Element Void m b -> ConduitT BS.ByteString Void m b -> m ()
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

      source .| awaitForever (liftIO . print)
      liftIO $ print "</stream> ;D"

--------------------------------------------------
-- Main server
--------------------------------------------------

main :: IO ()
main = do
  putStrLn "Running SQL migrations..."
  runSqlite (xmppDB def) $ runMigration migrateAll
  putStrLn "Starting server..."

  -- Generate channel map.
  cm <- atomically $ empty
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
  chan <- liftIO $ newTMChanIO
--  streamId <- liftIO $ randomIO

  let tmSource = sourceTMChan chan
  let tmSink   = sinkTMChan chan
--  let docond = do v <- await; yield (fromJust v); liftIO $ print "test"; liftIO $ print v; docond

  forkIO $ (runConduit $ tmSource .| renderElements .| sink)
--  forkIO $ chanToSink chan sink
  forkIO $ (runConduit $ yield testElement .| tmSink)


testElement = Element "message" (M.fromList [("to", "foo"), ("from", "calvin")]) []
testDocument = Document emptyPrologue testElement []
