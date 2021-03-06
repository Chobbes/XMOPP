{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Messages where

import Conduit

import Data.Text (Text, unpack)

import qualified Data.Map as M

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))

import Control.Monad.Logger

import XMPP
import InternalMessaging

-- | Conduit to receive messages, and then forward them to a message handler.
receiveMessage
  :: MonadThrow m =>
     (Text -> Text -> Text -> Text -> ConduitT Event o m r)
     -> ConduitT Event o m (Maybe r)
receiveMessage handler =
  tag' "{jabber:client}message" attrs (\(t,f,i,ty) -> handler t f i ty)
  where
    attrs = (,,,) <$> requireAttr "to"
            <*> requireAttr "from"
            <*> requireAttr "id"
            <*> requireAttr "type"
            <* ignoreAttrs

-- | Basic message handler. Collects messages, sends them to jid.
messageHandler
  :: (MonadThrow m, MonadIO m, MonadLogger m) =>
     ChanMap -> Text -> Text -> Text -> Text -> ConduitT Event o m ()
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
        Element "{jabber:client}message" (M.fromList [("from", from), ("to", to), ("id", i), ("type", ty)])
        [ NodeElement (Element "{jabber:client}body" mempty [NodeContent body])
        , NodeElement (Element "{urn:xmpp:sid:0}origin-id" (M.fromList [("id", i)]) [])
        , NodeElement (Element "{urn:xmpp:receipts}request" mempty [])
        , NodeElement (Element "{jabber:client}thread" mempty [NodeContent threadId])
        ]

  return (mkElem <$> body <*> threadId)
