module Concurrency where

{- Module containing some concurrency utilities.

  Most of this is for managing the sink to the client which needs to
  be synchronized.
-}

import GHC.Conc (atomically)

import Data.Conduit
import Data.Conduit.TMChan
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.Map as STC
import Data.Hashable

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class

import XMLRender
import Text.XML

import Data.Text
import qualified Data.ByteString as BS
import UnliftIO.Concurrent

import XMPP


-- | Fork a sink!
--
-- Given a sink, create a new sink which goes through a channel, and
-- then fork a thread which reads from the channel and sends output
-- over the sink.>
--
-- Useful when you want the sink to by synchronized, so the sink can
-- be written to on multiple threads without interleaving.
--
-- This also returns the synchronization channel.
forkSink
  :: (MonadUnliftIO m1, MonadIO m3) =>
     ConduitT BS.ByteString Void m1 ()
     -> m1 (ConduitT Element z m3 (), TMChan Element)
forkSink sink = tmSink (\src -> runConduit $ src .| renderElements .| sink)


-- TODO: needs a better name.
-- | Create a synchronized sink which will be forwarded to some handler.
tmSink
  :: (MonadUnliftIO m1, MonadIO m2, MonadIO m3) =>
     (ConduitT () a m2 () -> m1 ()) -> m1 (ConduitT a z m3 (), TMChan a)
tmSink handler = do
  chan <- liftIO newTMChanIO

  let tmSource = sourceTMChan chan
  let tmSink   = sinkTMChan chan

  forkIO $ handler tmSource
  return (tmSink, chan)

-- TODO: Need a way for channels to remove themselves???
-- Probably on end of stream...
allocateChannel
  :: (MonadIO m, Eq k1, Hashable k1, Eq k2, Hashable k2) =>
     Map k1 ([k2], Map k2 (TMChan a)) -> k1 -> k2 -> m (TMChan a)
allocateChannel cm key resource = do
  chan <- liftIO newTMChanIO
  liftIO $ atomically $ do
    mm <- STC.lookup key cm
    case mm of
      Nothing -> do
        m <- STC.empty
        STC.insert resource chan m
        STC.insert key ([resource], m) cm
      Just (rs, m) -> do
        STC.insert resource chan m
        STC.insert key (resource:rs, m) cm
  return chan

createHandledChannel
  :: (MonadIO (t m), Eq k1, Hashable k1, Eq k2, Hashable k2, MonadTrans t, Monad m, MonadUnliftIO m) =>
     Map k1 ([k2], Map k2 (TMChan a)) -> k1 -> k2 -> (TMChan a -> m b) -> t m ()
createHandledChannel cm key resource handler = do
  chan <- allocateChannel cm key resource
  void . lift . forkIO . void $ handler chan