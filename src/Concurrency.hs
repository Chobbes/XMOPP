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
-- over the sink.
--
-- Useful when you want the sink to be synchronized, so the sink can
-- be written to on multiple threads without interleaving.
--
-- This also returns the synchronization channel.
forkSink
  :: (MonadUnliftIO m1, MonadIO m2) =>
     ConduitT BS.ByteString Void m1 ()
     -> m1 (ConduitT Element o m2 (), TMChan Element)
forkSink sink = tmSink (\src -> runConduit $ src .| renderElements .| sink)


-- TODO: needs a better name.
-- | Create a synchronized sink which will be forwarded to some handler.
tmSink
  :: (MonadUnliftIO m1, MonadIO m2, MonadIO m3) =>
     (ConduitT () i m2 () -> m1 ()) -> m1 (ConduitT i o m3 (), TMChan i)
tmSink handler = do
  chan <- liftIO newTMChanIO

  let tmSource = sourceTMChan chan
  let tmSink   = sinkTMChan chan

  forkIO $ handler tmSource
  return (tmSink, chan)

-- TODO: Need a way for channels to remove themselves???
allocateChannel
  :: (MonadIO m, Eq k, Hashable k) =>
     Map k [TMChan a] -> k -> m (TMChan a)
allocateChannel cm resource = do
  chan <- liftIO newTMChanIO
  liftIO $ atomically $ do
    cs <- STC.lookup resource cm
    case cs of
      Nothing -> STC.insert resource [chan] cm
      Just cs -> STC.insert resource (chan:cs) cm
  return chan

createHandledChannel
  :: (MonadIO (t m), Eq k, Hashable k, MonadTrans t, Monad m, MonadUnliftIO m) =>
     Map k [TMChan a] -> k -> (TMChan a -> m b) -> t m ()
createHandledChannel cm resource handler = do
  chan <- allocateChannel cm resource
  void . lift . forkIO . void $ handler chan
