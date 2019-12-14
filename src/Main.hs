{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Conduit
import Data.Conduit

import Data.Conduit.Network
import Data.Conduit.Network.TLS

import Data.Text (Text, unpack, pack)
import qualified Data.ByteString as BS

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))

import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import GHC.Conc (atomically, forkIO, STM)
import Control.Concurrent.STM.Map as STC

import Database.Persist.Sqlite

import Users
import XMLRender
import Concurrency
import XMPP
import Stream
import TLS
import SASL
import Iq
import Messages
import Presence
import Logging

-- | Construct a jid from an fqdn and a user.
userJid :: Text -> User -> JID
userJid fqdn u = userName u <> "@" <> fqdn

--------------------------------------------------
-- XMPP Stanzas
--------------------------------------------------

-- | Todo, figure out how to allow for stream restarts at any point.
-- This should be architected more like a state machine.
handleClient
  :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m,
      MonadUnliftIO m, MonadLogger m) =>
     StreamEventHandler m () Void () () ->
     ChanMap -> AppData -> m ()
handleClient handleStreamEvents cm ad =
  do (sink, chan) <- liftIO $ forkSink (appSink ad)
     handleClient' handleStreamEvents cm (appSource ad .| parseBytes def) sink (appSink ad)

-- Separated for testing
handleClient'
   :: (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m, MonadUnliftIO m, MonadLogger m, Show r) =>
   StreamEventHandler m () Void r () ->
   ChanMap ->
   ConduitT () Event m () ->
   ConduitT Element Void m r ->
   ConduitT BS.ByteString Void m r ->
   m ()
handleClient' handleStreamEvents cm source sink bytesink = runConduit $ do
  streamid <- openStreamIO source bytesink

  -- Get user and pass
  auth <- plainAuth source sink
  case auth of
    Nothing -> do
      yield notAuthorized .| sink
      logErrorN "Authentication failed."
    Just u  -> do
      yield success .| sink
      logDebugN $ "User authenticated: " <> pack (show auth)

      -- Restart stream and present bind feature.
      openStreamIO source bytesink
      yield bindFeatures .| sink

      fqdn <- asks fqdn
      let jid = userJid fqdn u

      resource <- source .| receiveIqBind (bindHandler cm jid sink)

      case resource of
        Nothing       -> logDebugN $ "Could not bind resource for: " <> jid
        Just resource -> do
          handleStreamEvents cm source sink jid

          -- Free channel. TODO: look into resourceT
          freeResource cm jid resource

      logDebugN $ "End of stream for: " <> jid

-- | Default handler for stream events (messaging, iq, and presence).
handleStreamDefault
  :: (MonadThrow m, MonadLogger m, MonadReader XMPPSettings m,
      MonadUnliftIO m) =>
     StreamEventHandler m i o r' r
handleStreamDefault cm source sink jid = do
  source .| choose [ receiveMessage (messageHandler cm)
                   , receiveIq (iqHandler cm (void sink))
                   , receivePresence (presenceHandler cm)
                   ]
  handleStreamDefault cm source sink jid


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
  runStderrLoggingT $ flip runReaderT def $ do
    port <- asks xmppPort
    runTCPServerStartTLS (tlsConfig "*" port "cert.pem" "key.pem") (xmpp cm)

-- | Main XMPP client handling.
xmpp ::
  ChanMap -> GeneralApplicationStartTLS XMPPMonad ()
xmpp cm (appData, stls) = do
  startTLSIO appData
  logDebugN "Starting TLS..."
  stls $ handleClient handleStreamDefault cm
