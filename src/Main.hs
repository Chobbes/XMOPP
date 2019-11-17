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
import qualified Data.ByteString.Char8 as BSC
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
import Test.HUnit
import Control.Monad.ST

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
     ConduitT a c m UUID
openStream source sink = do
  source .| parseBytes def .| awaitStream
  initiateStream sink

-- | Todo, figure out how to allow for stream restarts at any point.
-- This should be architected more like a state machine.
handleClient :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadUnliftIO m) => AppData -> m ()
handleClient ad = runConduit $ do
  streamid <- openStream (appSource ad) (appSink ad)

  -- Get user and pass
  auth <- plainAuth (appSource ad) (appSink ad)
  case auth of
    Nothing -> do
      notAuthorized .| XR.renderBytes def .| appSink ad
      liftIO $ putStrLn "Authentication failed."
    Just _  -> do
      success .| XR.renderBytes def .| appSink ad
      liftIO $ print auth

      -- Restart stream and present bind feature.
      openStream (appSource ad) (appSink ad)
      bindFeatures .| XR.renderBytes def .| appSink ad

      iq <- appSource ad .| parseBytes def .| receiveIq
      liftIO $ print iq

      yield (encodeUtf8 . hack . iqId $ fromJust iq) .| appSink ad

-- TODO replace this
hack :: Text -> Text
hack id =
  mconcat [ "<iq xmlns=\"jabber:client\" id=\"", id, "\" type=\"result\">"
          , " <bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\">"
          , " <jid>test@localhost/gajim.CD9NEZ09</jid>"
          , " </bind>"
          , " </iq>"
          ]

--------------------------------------------------
-- TLS
--------------------------------------------------

tlsNamespace :: Text
tlsNamespace = "urn:ietf:params:xml:ns:xmpp-tls"

-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLS :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) => AppData -> m ()
startTLS ad = runConduit $ do
  openStream (appSource ad) (appSink ad)

  -- Send StartTLS feature.
  tlsFeatures .| XR.renderBytes def .| appSink ad

  -- Wait for TLS request from client.
  liftIO $ putStrLn "Awaiting TLS"
  starttls <- appSource ad .| parseBytes def .| awaitStartTls

  -- Tell client to proceed
  liftIO $ putStrLn "Sending TLS proceed"
  proceed .| XR.renderBytes def .| appSink ad
  liftIO $ putStrLn "Closing unencrypted channel."

proceed :: Monad m => ConduitT i Event m ()
proceed = XR.tag (Name "proceed" (Just tlsNamespace) Nothing) mempty (return ())

tlsFeatures :: Monad m => ConduitT i Event m ()
tlsFeatures = features tlsFeature
  where
    tlsFeature = XR.tag tlsName mempty required
    tlsName    = Name "starttls" (Just tlsNamespace)
                                 Nothing--(Just "stream") -- if I remove the prefix here then required gets an empty namespace?????

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

failure :: Monad m => ConduitT i Event m () -> ConduitT i Event m ()
failure = XR.tag (Name "failure" (Just saslNamespace) Nothing) mempty

-- TODO on receiving abort we need to reply with this
aborted :: Monad m => ConduitT i Event m ()
aborted = failure $ XR.tag "aborted" mempty (return ())

notAuthorized :: Monad m => ConduitT i Event m ()
notAuthorized = failure $ XR.tag "not-authorized" mempty (return ())

success :: Monad m => ConduitT i Event m ()
success = XR.tag (Name "success" (Just saslNamespace) Nothing) mempty (return ())

authFeatures :: Monad m => ConduitT i Event m ()
authFeatures = features mechanisms
  where
    mechanisms = XR.tag mechanismsName mempty plain
    mechanismsName = Name "mechanisms"
                          (Just saslNamespace)
                          Nothing
    plain = XR.tag (Name "mechanism" Nothing Nothing) mempty (XR.content "PLAIN")

awaitAuth :: MonadThrow m => ConduitT Event a m (Maybe (Text, Text))
awaitAuth = do
  authStr <- tagIgnoreAttrs (matching (==(Name {nameLocalName = "auth", nameNamespace = Just saslNamespace, namePrefix = Nothing}))) content -- TODO check for mechanism='PLAIN'
  return $ do
    auth <- authStr
    case decodeUtf8 <$> (BS.split 0 . decodeLenient $ encodeUtf8 auth) of
      [_, user, pass] -> return (user, pass)
      _               -> Nothing

--------------------------------------------------
-- OTHER STUFF
--------------------------------------------------

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

features :: Monad m => ConduitT i Event m () -> ConduitT i Event m ()
features = XR.tag featureName mempty
  where
    featureName = Name "features"
                       (Just "http://etherx.jabber.org/streams")
                       (Just "stream")

required :: Monad m => ConduitT i Event m ()
required = XR.tag "required" mempty (return ())

bindFeatures :: Monad m => ConduitT i Event m ()
bindFeatures = features bind
  where
    bind = XR.tag bindName mempty required
    bindName = Name "bind" (Just "urn:ietf:params:xml:ns:xmpp-bind") Nothing

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
  runSqlite (xmppDB def) $ runMigration migrateAll
  runReaderT (do port <- asks xmppPort
                 runTCPServerStartTLS (tlsConfig "*" port "cert.pem" "key.pem") xmpp) def


xmpp :: (PrimMonad m, MonadReader XMPPSettings m, MonadIO m, MonadUnliftIO m, MonadThrow m) => (AppData, (AppData -> m ()) -> m a) -> m a
xmpp (appData, stls) = do
  startTLS appData
  liftIO $ putStrLn "Starting TLS..."
  stls handleClient

-- TODO: move tests to separate file

test_required :: Test
test_required = runST (runConduit $ required .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<required/>"])

test_proceed :: Test
test_proceed = runST (runConduit $ proceed .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>"])

test_success :: Test
test_success = runST (runConduit $ success .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<success xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"])

-- The following tests don't pass.
-- Some weirdness with XR.tag, prefixes, and namespaces.
-- Doesn't match the spec, but seems to be ok for the client.

test_aborted :: Test
test_aborted = runST (runConduit $ aborted .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><aborted/></failure>"])

test_tlsFeatures :: Test
test_tlsFeatures = runST (runConduit $ tlsFeatures .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<stream:features><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required/></starttls></stream:features>"])

test_authFeatures :: Test
test_authFeatures = runST (runConduit $ authFeatures .| XR.renderBytes def .| consume) ~?= ([BSC.pack "<stream:features><mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><mechanism>PLAIN</mechanism></mechanisms></stream:features>"])
