{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module TLS where

import Data.Conduit
import Data.Conduit.Network
import Data.Text (Text)
import Data.Default
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Primitive
import Data.XML.Types (Event(..), Content(..))
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.ByteString as BS

import XMPP
import Stream
import Concurrency

tlsNamespace :: Text
tlsNamespace = "urn:ietf:params:xml:ns:xmpp-tls"

-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLS :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) => AppData -> m ()
startTLS ad = do
  (sink, chan) <- liftIO $ forkSink (appSink ad)
  startTLS' (appSource ad .| parseBytes def) sink (appSink ad)

startTLS' :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m) =>
  ConduitT () Event m () -> ConduitT Element Void m () -> ConduitT BS.ByteString Void m () -> m ()
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
