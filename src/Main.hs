{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Network
import Data.Conduit.Network.TLS
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.XML.Types
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Data.Conduit.List as CL
import Data.Char

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
awaitStream = do
  element <- await
  case element of
    Just (EventBeginElement n ats) ->
      if n == (Name {nameLocalName = "stream", nameNamespace = Just "http://etherx.jabber.org/streams", namePrefix = Just "stream"})
        then return element
        else awaitStream
    Nothing -> return Nothing
    _ -> awaitStream

awaitStartTls :: MonadThrow m => ConduitT Event a m (Maybe Event)
awaitStartTls = do
  element <- await
  case element of
    Just (EventBeginElement n ats) ->
      if n == (Name {nameLocalName = "starttls", nameNamespace = Just "urn:ietf:params:xml:ns:xmpp-tls", namePrefix = Nothing})
        then return element
        else awaitStartTls
    Nothing -> return Nothing
    _ -> awaitStartTls

main :: IO ()
main = do
--  forkTCPServer settings handleClient
  runTCPServerStartTLS (tlsConfig "*" xmppPort "cert.pem" "key.pem") $
    \(appData, stls) -> do
      startTLS appData
      liftIO $ putStrLn "Starting TLS..."
      stls $ handleClient
