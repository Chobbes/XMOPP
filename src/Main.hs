{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Conduit
import Data.Conduit

import Data.Conduit.Network
import Data.Conduit.Network.TLS

import Data.Text (Text, unpack)
import qualified Data.ByteString as BS

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))

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

--------------------------------------------------
-- XMPP Stanzas
--------------------------------------------------

userJid :: Text -> User -> Text
userJid fqdn u = userName u <> "@" <> fqdn

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
