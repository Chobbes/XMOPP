{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Presence where

import Conduit

import Data.Text (Text, unpack)

import qualified Data.Map as M

import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))

import XMPP
import InternalMessaging

-- | Conduit to receive presence stanzas, and then forward them to a presence handler.
-- receivePresence
--   :: MonadThrow m =>
--      (Text -> Text -> Text -> Text -> ConduitT Event o m c)
--      -> ConduitT Event o m (Maybe c)
receivePresence handler =
  tag' "{jabber:client}presence" attrs handler
  where
    attrs = requireAttr "from" <* ignoreAttrs

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
