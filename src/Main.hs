{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}  -- Persistent :(?
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
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Data.Conduit.List as CL
import Data.Char
import qualified Data.ByteString as BS
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
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

--------------------------------------------------
-- Global XMPP settings
--------------------------------------------------

-- | Stream version?
version :: Text
version = "1.0"

data XMPPSettings =
  XMPPSettings { fqdn     :: Text
               , xmppPort :: Int
               , xmppDB   :: Text
               }

instance Default XMPPSettings where
  def = XMPPSettings "localhost" 5222 "xmpp.db"

--------------------------------------------------
-- Database
--------------------------------------------------

-- TODO make sure passwords aren't plain text in the future.
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password Text
    UniqueName name
    deriving Show
|]

-- TODO wrap AppData / configuration in a reader monad?

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m) => ConduitT BS.ByteString o m r -> ConduitT i o m UUID
initiateStream sink = do
    streamId <- liftIO randomIO
    fqdn <- asks fqdn
    streamRespHeader fqdn "en" streamId  .| XR.renderBytes def .| sink
    return streamId

-- | Open a stream.
openStream
  :: (MonadThrow m, PrimMonad m, MonadIO m, MonadReader XMPPSettings m) =>
     ConduitM a BS.ByteString m () ->
     ConduitT BS.ByteString c m r ->
     ConduitT a c m (Maybe Event)
openStream source sink = do
  stream <- source .| parseBytes def .| awaitStream
  initiateStream sink
  return stream

-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLS :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) => AppData -> m ()
startTLS ad = runConduit $ do
  openStream (appSource ad) (appSink ad)

  -- Send StartTLS feature.
  featuresTLS  .| XR.renderBytes def .| appSink ad

  -- Wait for TLS request from client.
  liftIO $ putStrLn "Awaiting TLS"
  starttls <- appSource ad .| parseBytes def .| awaitStartTls

  -- Tell client to proceed
  liftIO $ putStrLn "Sending TLS proceed"
  yield "<proceed xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>" .| appSink ad
  liftIO $ putStrLn "Closing unencrypted channel."

-- | Todo, figure out how to allow for stream restarts at any point.
-- This should be architected more like a state machine.
handleClient :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadUnliftIO m) => AppData -> m ()
handleClient ad = runConduit $ do
  openStream (appSource ad) (appSink ad)

  -- Get user and pass
  auth <- plainAuth (appSource ad) (appSink ad)
  case auth of
    Nothing -> return () -- TODO: send error
    Just _  -> do
      yield "<success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'></success>" .| appSink ad
      liftIO $ print auth

      -- Restart stream and present bind feature.
      openStream (appSource ad) (appSink ad)
      bindFeatures .| XR.renderBytes def .| appSink ad

      iq <- appSource ad .| parseBytes def .| receiveIq
      liftIO $ print iq

-- | Get authentication information.
plainAuth
  :: (PrimMonad m, MonadThrow m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
     ConduitM a1 BS.ByteString m ()
     -> ConduitM BS.ByteString c m a2
     -> ConduitT a1 c m (Maybe User)
plainAuth source sink = do
  authFeatures .| XR.renderBytes def .| sink
  auth <- source .| parseBytes def .| awaitAuth
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

streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId =
  yield $ EventBeginElement streamName attrs
  where attrs = [ at "from" from
                , at "version" version
                , at "id" (toText streamId)
                , (Name "lang" (Just "xml") (Just "xml"), [ContentText lang])
                ]
        at name content = (Name name Nothing Nothing, [ContentText content])
        streamName = Name "stream"
                          (Just "http://etherx.jabber.org/streams")
                          (Just "stream")

features :: Monad m => ConduitT i Event m () -> ConduitT i Event m ()
features = XR.tag featureName mempty
  where
    featureName = Name "features" (Just "http://etherx.jabber.org/streams") (Just "stream")

featuresTLS :: Monad m => ConduitT i Event m ()
featuresTLS = features tlsFeature
  where
    tlsFeature = XR.tag tlsName (XR.attr "xmlns" "jabber:client") required
    tlsName    = Name "starttls" (Just "urn:ietf:params:xml:ns:xmpp-tls")
                                 (Just "stream")

required :: Monad m => ConduitT i Event m ()
required = XR.tag "required" mempty (return ())

bindFeatures :: Monad m => ConduitT i Event m ()
bindFeatures = features bind
  where
    bind = XR.tag bindName mempty required
    bindName = Name "bind" (Just "urn:ietf:params:xml:ns:xmpp-bind") Nothing

authFeatures :: Monad m => ConduitT i Event m ()
authFeatures = features mechanisms
  where
    mechanisms = XR.tag mechanismsName mempty plain
    mechanismsName = Name "mechanisms"
                          (Just "urn:ietf:params:xml:ns:xmpp-sasl")
                          Nothing
    plain = XR.tag "mechanism" mempty (XR.content "PLAIN")

awaitStream :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStream = awaitName (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})

awaitStartTls :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStartTls = awaitName (Name {nameLocalName = "starttls", nameNamespace = Just "urn:ietf:params:xml:ns:xmpp-tls", namePrefix = Nothing})

awaitName :: MonadThrow m => Name -> ConduitT Event a m (Maybe Event)
awaitName name = do
  element <- await
  case element of
    Just (EventBeginElement n ats) ->
      if n == name
        then return element
        else awaitName name
    Nothing -> return Nothing
    _ -> awaitName name

awaitAuth :: MonadThrow m => ConduitT Event a m (Maybe (Text, Text))
awaitAuth = do
  authStr <- tagIgnoreAttrs (matching (==(Name {nameLocalName = "auth", nameNamespace = Just "urn:ietf:params:xml:ns:xmpp-sasl", namePrefix = Nothing}))) content
  return $ do
    auth <- authStr
    case decodeUtf8 <$> (BS.split 0 . decodeLenient $ encodeUtf8 auth) of
      [_, user, pass] -> return (user, pass)
      _               -> Nothing

awaitBind :: MonadThrow m => ConduitT Event a m (Maybe Text)
awaitBind = undefined

--------------------------------------------------
-- XMPP Stanzas
--------------------------------------------------

data IqStanza = MkIq { iqId   :: Text
                     , iqType :: Text
                     , iqContents :: [Event]  -- Change to a full XML document??
                     }
                deriving (Eq, Show)


receiveIq :: MonadThrow m => ConduitT Event a m (Maybe IqStanza)
receiveIq =
  tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type") iq
  where iq (i,t) = do
          c <- void takeAnyTreeContent .| consume
          return $ MkIq i t c


--------------------------------------------------
-- Main server
--------------------------------------------------

main :: IO ()
main = do
  runSqlite "test.db" $ runMigration migrateAll
  runReaderT (do port <- asks xmppPort
                 runTCPServerStartTLS (tlsConfig "*" port "cert.pem" "key.pem") xmpp) def


xmpp :: (PrimMonad m, MonadReader XMPPSettings m, MonadIO m, MonadUnliftIO m, MonadThrow m) => (AppData, (AppData -> m ()) -> m a) -> m a
xmpp (appData, stls) = do
  startTLS appData
  liftIO $ putStrLn "Starting TLS..."
  stls handleClient
