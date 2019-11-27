{-# LANGUAGE TupleSections              #-}
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
import qualified Data.XML.Types as XT
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
import Control.Concurrent (ThreadId)
import Control.Concurrent.Thread.Delay
import Data.Hashable

import Users
import XMLRender
import Concurrency
import XMPP
import Stream
import TLS
import SASL
import Iq

--------------------------------------------------
-- XMPP Stanzas
--------------------------------------------------

receiveMessage handler =
  tag' "{jabber:client}message" ((,,,) <$> requireAttr "to" <*> requireAttr "from" <*> requireAttr "id" <*> requireAttr "type" <* ignoreAttrs) (\(t,f,i,ty) -> handler t f i ty)

messageHandler cm to from i ty = do
  -- Read message contents
  elem <- choose [messageBody to from i ty]
  maybe (return ()) (sendToJid cm to) elem

messageBody to from i ty = do
  body <- tagIgnoreAttrs "{jabber:client}body" content
  ignoreAnyTreeContent  -- Ignore origin-id, already have information from it.
  ignoreAnyTreeContent  -- Also don't need any information from request tag.
  threadId <- tagIgnoreAttrs "{jabber:client}thread" content

  let mkElem body threadId =
        Element "{jabber:client}message" (M.fromList [("from", from), ("to", to), ("type", ty)])
        [ NodeElement (Element "{jabber:client}body" mempty [NodeContent body])
        , NodeElement (Element "{urn:xmpp:sid:0}origin-id" (M.fromList [("id", i)]) [])
        , NodeElement (Element "{urn:xmpp:receipts}request" mempty [])
        , NodeElement (Element "{jabber:client}thread" mempty [NodeContent threadId])
        ]

  return (mkElem <$> body <*> threadId)

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
--
-- If the jid has a resource as well, only send to that specific resource.
sendToJid
  :: MonadIO m =>
     ChanMap -> Text -> Element -> m ()
sendToJid cm to elem =
  case T.splitOn "/" to of
    [jid]           -> sendToJidAll cm jid elem
    [jid, resource] -> sendToResource cm jid resource elem
    (jid:_)         -> sendToJidAll cm jid elem
    _               -> return ()

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
sendToJidAll
  :: MonadIO m =>
     ChanMap -> Text -> Element -> m ()
sendToJidAll cm to elem = do
  channels <- liftIO . atomically $ do
    mm <- STC.lookup to cm
    case mm of
      Nothing      -> return []
      Just (rs, m) -> forM rs $ \r -> do
        maybeChan <- STC.lookup r m
        return (fromMaybe [] (fmap (:[]) maybeChan))
  liftIO $ putStrLn $ "Message to: " ++ show to
  liftIO $ print $ length channels
  writeToAllChannels (Prelude.concat channels) elem

sendToResource :: MonadIO m =>
     ChanMap -> Text -> Text -> Element -> m ()
sendToResource cm jid resource elem = do
  mchan <- liftIO . atomically $ do
    mm <- STC.lookup jid cm
    case mm of
      Nothing     -> return Nothing
      Just (_, m) -> STC.lookup resource m
  case mchan of
    Nothing   -> return ()
    Just chan -> liftIO . atomically $ writeTMChan chan elem

writeToAllChannels
  :: MonadIO m => [TMChan a] -> a -> m ()
writeToAllChannels channels elem =
  forM_ channels $ \chan -> liftIO . atomically $ writeTMChan chan elem

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
