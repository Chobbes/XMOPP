{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Iq where

import Conduit
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Network
import Data.Text (Text)
import Data.Default
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Primitive
import Control.Monad.IO.Unlift
import Data.XML.Types (Event(..), Content(..))
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.ByteString as BS
import qualified Data.Map as M

import XMPP
import Stream
import Concurrency

bindNamespace :: Text
bindNamespace = "urn:ietf:params:xml:ns:xmpp-bind"

bindName :: Name
bindName = Name "bind" (Just bindNamespace) Nothing

bindFeatures :: Element
bindFeatures = features [NodeElement bind]
  where
    bind = Element bindName mempty []

iqShort :: Text -> Text -> [Node] -> Element
iqShort i t = Element "iq" attrs
  where attrs = M.fromList [("id", i), ("type", t)]

bind :: Text -> Element
bind jid = Element bindName mempty [NodeElement bindElement]
  where
    bindElement = Element "jid" mempty [NodeContent jid]

receiveIqBind :: MonadThrow m =>
  (Text -> Text -> ConduitT Event o m r) -> ConduitT Event o m (Maybe r)
receiveIqBind handler =
  tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type") $ uncurry handler

bindHandler :: (MonadThrow m, PrimMonad m, MonadIO m, MonadUnliftIO m) =>
  ChanMap ->
  Text ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
bindHandler cm jid sink i t =
  if t /= "set"
  then error "type =/= set" -- TODO better error handling?
  else tagIgnoreAttrs (matching (==bindName)) doBind
  where
    resourceName = Name "resource" (Just bindNamespace) Nothing
    doBind = do
      resource <- tagIgnoreAttrs (matching (==resourceName)) content

      case resource of
        Nothing -> error "bad resource" -- TODO replace this
        Just resource -> do
          let fullResource = jid <> "/" <> resource
          let iqNodes      = [NodeElement (bind fullResource)]

          createHandledChannel cm jid resource (forwardHandler sink)
          yield (iqShort i "result" iqNodes) .| sink

infoNamespace :: Text
infoNamespace = "http://jabber.org/protocol/disco#info"

itemsNamespace :: Text
itemsNamespace = "http://jabber.org/protocol/disco#items"

iqName :: Name
iqName = Name {nameLocalName = "iq", nameNamespace = Just "jabber:client", namePrefix = Nothing}

testIqInfo :: BS.ByteString
testIqInfo = "<iq type='get' from='romeo@montague.net/orchard' to='plays.shakespeare.lit' id='info1'> <query xmlns='http://jabber.org/protocol/disco#info'/> </iq>"

iq :: Text -> Text -> Text -> Text -> [Node] -> Element
iq i t to from = Element "iq" attrs
  where attrs = M.fromList [("id", i), ("type", t), ("to", to), ("from", from)]

receiveIq :: MonadThrow m =>
  (Text -> Text -> Text -> Text -> ConduitT Event o m r) -> ConduitT Event o m (Maybe r)
receiveIq handler =
  tag' (matching (==iqName)) ((,,,) <$>
              requireAttr "id" <*>
              requireAttr "type" <*>
              requireAttr "to" <*>
              requireAttr "from") $ uncurry4 handler
  where
    uncurry4 f (w, x, y, z) = f w x y z

iqHandler :: (MonadThrow m, MonadIO m) =>
  ChanMap ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
iqHandler cm sink i t to from =
  if t /= "get"
  then error "type =/= get" -- TODO
  else do
    query <- tagNoAttr (matching (==infoQueryName)) content

    case query of -- TODO how to refactor
      Nothing -> do
        query <- tagNoAttr (matching (==itemsQueryName)) content
        case query of
          Nothing -> return Nothing
          Just q -> doItems
      Just q -> doInfo
  where
    infoQueryName = Name "query" (Just infoNamespace) Nothing
    itemsQueryName = Name "query" (Just itemsNamespace) Nothing
    doInfo = do
      r <- yield (iq i "result" from to [NodeElement (Element infoQueryName mempty [])]) .| sink
      return $ Just r
    doItems = do
      r <- yield (iq i "result1" from to [NodeElement (Element itemsQueryName mempty [])]) .| sink
      return $ Just r



data IqStanza = MkIq { iqId   :: Text
                     , iqType :: Text
                     , iqContents :: [Event]  -- Change to a full XML document??
                     }
                deriving (Eq, Show)

baseIqHandler i t = do
  c <- void takeAnyTreeContent .| consume
  return $ MkIq i t c