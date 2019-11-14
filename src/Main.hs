{-# LANGUAGE OverloadedStrings #-}
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

version :: Text
version = "1.0"

fqdn :: Text
fqdn = "localhost"

xmppPort :: Int
xmppPort = 5222

xmppTlsPort :: Int
xmppTlsPort = 5223

settings :: ServerSettings
settings = serverSettings xmppPort "*"

-- TODO wrap AppData / configuration in a reader monad.

-- | Generate a new initial stream header with a new UUID.
initiateStream :: (PrimMonad m, MonadIO m) => ConduitT BS.ByteString o m r -> ConduitT i o m UUID
initiateStream sink = do
    streamId <- liftIO randomIO
    streamRespHeader fqdn "en" streamId  .| XR.renderBytes def .| sink
    return streamId


-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLS :: AppData -> IO ()
startTLS ad = runConduit $ do
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ putStrLn "New connection"

  -- Send initial stream header in response
  initiateStream $ appSink ad

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
handleClient :: AppData -> IO ()
handleClient ad = runConduit $ do
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ putStrLn "New TLS connection."
  yield authResp .| appSink ad
  liftIO $ print stream
  auth <- appSource ad .| parseBytes def .| awaitAuth
  liftIO $ print auth
  -- TODO. Fix this. Just accepts, lol.
  yield "<success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'></success>" .| appSink ad
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ print stream
  yield (streamResp' (encodeUtf8 . fst $ fromJust auth)) .| appSink ad  -- TODO: Get rid of fromJust
  appSource ad .| parseBytes def .| awaitForever (lift . print)
  liftIO $ putStrLn "wah"

-- runConduit $ streamRespXML "localhost" "en" "eoauthoahu" .| XR.renderText def .| await
streamRespHeader :: Monad m => Text -> Text -> UUID -> ConduitT i Event m ()
streamRespHeader from lang streamId = do
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
features children = XR.tag featureName mempty children
  where
    featureName = Name "features" (Just "http://etherx.jabber.org/streams") (Just "stream")

featuresTLS :: Monad m => ConduitT i Event m ()
featuresTLS = features tlsFeature
  where
    tlsFeature = XR.tag tlsName (XR.attr "xmlns" "jabber:client") required
    tlsName    = Name "starttls" (Just "urn:ietf:params:xml:ns:xmpp-tls")
                                 (Just "stream")
    required   = XR.tag "required" mempty (return ())

-- \ <features xml:lang='en'  \
-- \           xmlns='jabber:client'  \
-- \           xmlns:stream='http://etherx.jabber.org/streams'> \
-- \ <starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"> \
-- \ <required /> \
-- \ </starttls> \
-- \ </features>"


-- TODO fix this. Generate id randomly.
streamResp :: BS.ByteString
streamResp =
  "<?xml version='1.0'?> \
\      <stream:stream  \
\          from='localhost'  \
\          id='FOO++TR84Sm6A3hnt3Q065SnAbbk3Y=' \
\          version='1.0'  \
\          xml:lang='en'  \
\          xmlns='jabber:client'  \
\          xmlns:stream='http://etherx.jabber.org/streams'> \
\ <features xml:lang='en'  \
\           xmlns='jabber:client'  \
\           xmlns:stream='http://etherx.jabber.org/streams'> \
\ <starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"> \
\ <required /> \
\ </starttls> \
\ </features>"

-- Escape jid?
streamResp' :: BS.ByteString -> BS.ByteString
streamResp' to =
  "<?xml version='1.0'?> \
\      <stream:stream \
\          from='localhost' \
\          id='TR84Sm6A3hnt3Q065SnAbbk3Y=' \
\          version='1.0' \
\          xml:lang='en' \
\          xmlns='jabber:client' \
\          xmlns:stream='http://etherx.jabber.org/streams'> \
\ <features xmlns=\"http://etherx.jabber.org/streams\"> \
\ <c hash=\"sha-1\" xmlns=\"http://jabber.org/protocol/caps\" ver=\"pp/B5UZAFTyIuLCYMfy+HDg8MSk=\" node=\"http://prosody.im\" /> \
\ <sm xmlns=\"urn:xmpp:sm:2\"> \
  \ <optional /> \
  \ </sm> \
  \ <sm xmlns=\"urn:xmpp:sm:3\"> \
  \ <optional /> \
  \ </sm> \
  \ <csi xmlns=\"urn:xmpp:csi:0\" /> \
  \ <bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"> \
  \ <required /> \
  \ </bind> \
  \ <session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\"> \
  \ <optional /> \
  \ </session> \
  \ <ver xmlns=\"urn:xmpp:features:rosterver\" /> \
  \ </features>"

-- \ <features xml:lang='en'  \
-- \           xmlns='jabber:client'  \
-- \           xmlns:stream='http://etherx.jabber.org/streams'> \
-- \ <bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"> \
-- \ <required /> \
-- \ </bind> \
-- \ </features>"

authResp =
  "<?xml version='1.0'?> \
\      <stream:stream  \
\          from='localhost'  \
\          id='FOO++TR84Sm6A3hnt3Q065SnAbbk3Y=' \
\          version='1.0'  \
\          xml:lang='en'  \
\          xmlns='jabber:client'  \
\          xmlns:stream='http://etherx.jabber.org/streams'> \
\ <features xml:lang='en'  \
\           xmlns='jabber:client'  \
\           xmlns:stream='http://etherx.jabber.org/streams'> \
\ <mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"> \
\ <mechanism>PLAIN</mechanism> \
\ </mechanisms> \
\ </features>"

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
--   <iq type="set" id="78d817dd-7058-46bc-a752-45e8079215d8">
-- <bind xmlns="urn:ietf:params:xml:ns:xmpp-bind">
-- <resource>gajim.MGATHP3J</resource>
-- </bind>
-- </iq>

-- <auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN">AHRlc3QAZm9vYmFy</auth>

main :: IO ()
main =
  runTCPServerStartTLS (tlsConfig "*" xmppPort "cert.pem" "key.pem") xmpp

xmpp :: (AppData, (AppData -> IO ()) -> IO a) -> IO a
xmpp (appData, stls) = do
  startTLS appData
  liftIO $ putStrLn "Starting TLS..."
  stls handleClient

