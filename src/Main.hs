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

xmppPort :: Int
xmppPort = 5222

xmppTlsPort :: Int
xmppTlsPort = 5223

settings :: ServerSettings
settings = serverSettings xmppPort "*"

startTLS :: AppData -> IO ()
startTLS ad = runConduit $ do
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ putStrLn "New connection"
  yield streamResp .| appSink ad
  liftIO $ putStrLn "Awaiting TLS"
  starttls <- appSource ad .| parseBytes def .| awaitStartTls
  liftIO $ putStrLn "Sending TLS proceed"
  yield "<proceed xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>" .| appSink ad
  liftIO $ putStrLn "Closing unencrypted channel."

handleClient :: AppData -> IO ()
handleClient ad = runConduit $ do
  stream <- appSource ad .| parseBytes def .| awaitStream
  liftIO $ putStrLn "New TLS connection."
  yield authResp .| appSink ad
  liftIO $ print stream
  auth <- appSource ad .| parseBytes def .| awaitAuth
  liftIO $ print auth
  appSource ad .| parseBytes def .| awaitForever (lift . print)
  liftIO $ putStrLn "wah"


-- TODO fix this. Generate id randomly.
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

-- <auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN">AHRlc3QAZm9vYmFy</auth>

main :: IO ()
main =
  runTCPServerStartTLS (tlsConfig "*" xmppPort "cert.pem" "key.pem") xmpp

xmpp :: (AppData, (AppData -> IO ()) -> IO a) -> IO a
xmpp (appData, stls) = do
  startTLS appData
  liftIO $ putStrLn "Starting TLS..."
  stls handleClient
