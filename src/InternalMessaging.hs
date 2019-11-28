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
import Data.Maybe

import Text.XML hiding (parseText)

import XMPP

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
        return $ maybeToList maybeChan
  liftIO $ putStrLn $ "Message to: " ++ show to
  liftIO $ print $ length channels
  writeToAllChannels (Prelude.concat channels) elem

-- | Send message to a *specific* XMPP resource.
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

-- | Write to a value to each channel in a list.
writeToAllChannels
  :: MonadIO m => [TMChan a] -> a -> m ()
writeToAllChannels channels elem =
  forM_ channels $ \chan -> liftIO . atomically $ writeTMChan chan elem
