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
import Control.Monad.Logger
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

tlsName :: Name
tlsName = Name {nameLocalName = "starttls", nameNamespace = Just tlsNamespace, namePrefix = Nothing}

awaitStartTls :: MonadThrow m => ConduitT Event o m (Maybe Event)
awaitStartTls = awaitName tlsName

-- | Handle the initial TLS stream negotiation from an XMPP client.
-- TODO, modify this to be able to skip garbage that we don't handle.
startTLSIO :: (PrimMonad m, MonadIO m, MonadReader XMPPSettings m, MonadThrow m, MonadLogger m) =>
  AppData -> m ()
startTLSIO ad = runConduit $ do
  (sink, chan) <- liftIO $ forkSink (appSink ad)
  openStreamIO source (appSink ad)
  startTLS source sink
  where
    source = appSource ad .| parseBytes def

startTLS :: (MonadLogger m, MonadThrow m) =>
  ConduitT () Event m () -> ConduitT Element Void m () -> ConduitT () Void m ()
startTLS source sink = do
  -- Send StartTLS feature.
  yield tlsFeatures .| sink

  -- Wait for TLS request from client.
  logDebugN "Awaiting TLS"
  starttls <- source .| awaitStartTls
  case starttls of
    Nothing -> logErrorN "TLS request not received"
    _ -> do
      -- Tell client to proceed
      logDebugN "Sending TLS proceed"
      yield proceed .| sink
      error "asdf"
      logDebugN "Closing unencrypted channel."

proceed :: Element
proceed = Element (Name "proceed" (Just tlsNamespace) Nothing) mempty []

tlsFeatures :: Element
tlsFeatures = features [NodeElement tlsFeature]
  where
    tlsFeature = Element tlsName mempty [NodeElement required]
