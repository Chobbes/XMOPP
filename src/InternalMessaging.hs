{-# LANGUAGE OverloadedStrings #-}
{- Messaging within the server -}
module InternalMessaging where

import Conduit
import Data.Conduit.TMChan

import GHC.Conc (atomically, forkIO, STM)
import Control.Concurrent.STM.Map as STC

import qualified Data.Text as T
import Data.Text (Text, unpack)

import Control.Monad
import Control.Monad.Logger

import Data.Maybe
import Data.Hashable

import Text.XML hiding (parseText)

import XMPP

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
--
-- If the jid has a resource as well, only send to that specific resource.
sendToJid :: (MonadIO m, MonadLogger m) =>
     ChanMap -> Text -> Element -> m ()
sendToJid cm to elem =
  case T.splitOn "/" to of
    [jid, resource] -> sendToResource cm jid resource elem
    (jid:_)         -> sendToJidAll cm jid elem
    _               -> return ()

-- | Look up channels associated with a given jid in the channel map, and send
-- an element over that channel.
sendToJidAll :: (MonadIO m, MonadLogger m) =>
     ChanMap -> JID -> Element -> m ()
sendToJidAll cm to elem = do
  channels <- liftIO . atomically $ getJidChannels cm to
  logDebugN $ "Message to: " <> to
  logDebugN $ "Channels for " <> to <> ": " <> T.pack (show $ length channels)
  writeToAllChannels channels elem

-- | Send message to a *specific* XMPP resource.
sendToResource :: MonadIO m => ChanMap -> JID -> XMPPResource -> Element -> m ()
sendToResource cm jid resource elem = do
  mchan <- liftIO . atomically $ do
    mm <- STC.lookup jid cm
    case mm of
      Nothing     -> return Nothing
      Just (_, _, m) -> STC.lookup resource m
  case mchan of
    Nothing   -> return ()
    Just chan -> liftIO . atomically $ writeTMChan chan elem

-- | Get channels associated with a jid
getJidChannels :: ChanMap -> JID -> STM [TMChan Element]
getJidChannels cm jid = do
  mm <- STC.lookup jid cm
  chans <- case mm of
             Nothing      -> return []
             Just (rs, _, m) -> forM rs $ \r -> do
               maybeChan <- STC.lookup r m
               return $ maybeToList maybeChan
  return $ Prelude.concat chans

-- | Write to a value to each channel in a list.
writeToAllChannels
  :: MonadIO m => [TMChan a] -> a -> m ()
writeToAllChannels channels elem =
  forM_ channels $ \chan -> liftIO . atomically $ writeTMChan chan elem

-- | Read a value from each channel in a list.
readAllChannels
  :: Traversable t => t (TMChan a) -> STM (t (Maybe a))
readAllChannels = mapM readTMChan
