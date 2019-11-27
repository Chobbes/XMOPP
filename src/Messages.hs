{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Messages where

import Conduit
import Data.Conduit.TMChan

import Control.Monad.Catch
import Control.Monad.IO.Class

import GHC.Conc (atomically, forkIO, STM)
import Control.Concurrent.STM.Map as STC

import qualified Data.Text as T
import Data.Text (Text, unpack)

import qualified Data.Map as M
import Control.Monad
import Data.Maybe

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))
import qualified Text.XML.Stream.Render as XR

import XMPP

-- | Conduit to receive messages, and then forward them to a message handler.
receiveMessage
  :: MonadThrow m =>
     (Text -> Text -> Text -> Text -> ConduitT Event o m c)
     -> ConduitT Event o m (Maybe c)
receiveMessage handler =
  tag' "{jabber:client}message" attrs (\(t,f,i,ty) -> handler t f i ty)
  where
    attrs = (,,,) <$> requireAttr "to"
            <*> requireAttr "from"
            <*> requireAttr "id"
            <*> requireAttr "type"
            <* ignoreAttrs

-- | Basic message handler. Collects messages, sends them to jid.
messageHandler cm to from i ty = do
  -- Read message contents
  elem <- choose [messageBody to from i ty]
  maybe (return ()) (sendToJid cm to) elem

-- | Parse a normal message with a body from an event stream.
messageBody
  :: MonadThrow m =>
     Text -> Text -> Text -> Text -> ConduitT Event o m (Maybe Element)
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
