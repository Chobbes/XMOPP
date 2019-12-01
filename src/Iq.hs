{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
module Iq where

import Conduit
import Data.UUID
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Network
import Data.Text (Text, pack)
import Data.Default
import System.Random
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger
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
import Utils

-- Bind stanzas

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
  (Text -> Text -> ConduitT Event o m (Maybe Text)) -> ConduitT Event o m (Maybe Text)
receiveIqBind handler =
  join <$> (tag' "iq" ((,) <$> requireAttr "id" <*> requireAttr "type") $ uncurry handler)

bindHandler :: (MonadThrow m, PrimMonad m, MonadUnliftIO m, MonadLogger m) =>
  ChanMap ->
  Text ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe Text)
bindHandler cm jid sink i t =
  if t /= "set"
  then do
    logErrorN "Expected iq stanza with type = set for resource binding, but type =/= set."
    return Nothing
  else join <$> tagIgnoreAttrs "{urn:ietf:params:xml:ns:xmpp-bind}bind" doBind
  where
    doBind = do
      r <- tagIgnoreAttrs "{urn:ietf:params:xml:ns:xmpp-bind}resource" content
      uuid <- liftIO (randomIO :: IO UUID)

      let resource = case r of
                       Nothing -> pack $ show uuid
                       Just r -> r
      let fullResource = jid <> "/" <> resource
      let iqNodes      = [NodeElement (bind fullResource)]

      createHandledChannel cm jid resource (forwardHandler sink)
      yield (iqShort i "result" iqNodes) .| sink
      return (Just resource)

-- "Normal" Iq stanzas

iqName :: Name
iqName = Name {nameLocalName = "iq", nameNamespace = Just "jabber:client", namePrefix = Nothing}

testIqInfo :: BS.ByteString
testIqInfo = "<iq type='get' from='romeo@montague.net/orchard' to='plays.shakespeare.lit' id='info1'> <query xmlns='http://jabber.org/protocol/disco#info'/> </iq>"

iq :: Text -> Text -> Text -> Text -> [Node] -> Element
iq i t to from = Element iqName attrs
  where attrs = M.fromList [("id", i), ("type", t), ("to", to), ("from", from)]

queryName :: Text -> Name
queryName namespace = Name {nameLocalName = "query", nameNamespace = Just namespace, namePrefix = Nothing}

query :: Text -> [Element] -> Element
query namespace nodes = Element (queryName namespace) mempty $ NodeElement <$> nodes

identity :: Text -> Text -> Text -> Element
identity category t name = Element "identity" attrs []
  where attrs = M.fromList [("category", category), ("type", t), ("name", name)]

feature :: Text -> Element
feature t = Element "feature" attrs []
  where attrs = M.fromList [("var", t)]

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

iqHandler :: (MonadThrow m, MonadLogger m) =>
  ChanMap ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
iqHandler cm sink i t to from =
  if t /= "get"
  then return Nothing
  else choose [ infoHandler sink i t to from content
              , itemsHandler sink i t to from content
              , pingHandler sink i t to from content
              , queryError sink i t to from ]

infoNamespace :: Text
infoNamespace = "http://jabber.org/protocol/disco#info"

infoHandler :: (MonadThrow m, MonadLogger m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m Text ->
  ConduitT Event o m (Maybe r)
infoHandler sink i t to from c = do
  q <- tagNoAttr (matching (==infoQueryName)) c
  case q of
    Just q -> do
      r <- yield (iq i "result" from to [NodeElement (query infoNamespace [identity "cat" "type" "name", feature infoNamespace, feature pingNamespace])]) .| sink
      return $ Just r
    Nothing -> return Nothing
  where
    infoQueryName = queryName infoNamespace

itemsNamespace :: Text
itemsNamespace = "http://jabber.org/protocol/disco#items"

itemsHandler :: (MonadThrow m, MonadLogger m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m Text ->
  ConduitT Event o m (Maybe r)
itemsHandler sink i t to from c = do
  q <- tagNoAttr (matching (==itemsQueryName)) c
  case q of
    Just q -> do
      r <- yield (iq i "result" from to [NodeElement (query itemsNamespace [])]) .| sink
      return $ Just r
    Nothing -> return Nothing
  where
    itemsQueryName = queryName itemsNamespace

pingNamespace :: Text
pingNamespace = "urn:xmpp:ping"

pingHandler :: (MonadThrow m, MonadLogger m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m Text ->
  ConduitT Event o m (Maybe r)
pingHandler sink i t to from c = do
  q <- tagNoAttr (matching (==pingName)) c
  case q of
    Just q -> do
      r <- yield (iq i "result" from to []) .| sink
      return $ Just r
    Nothing -> return Nothing
  where
    pingName = Name "ping" (Just pingNamespace) Nothing

queryError :: (MonadThrow m, MonadLogger m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
queryError sink i t to from = do
  ignoreAnyTreeContent
  logDebugN $ "IQ error: " <> pack (show errorElem)
  r <- yield errorElem .| sink
  return $ Just r
  where
    errorElem = iq i "error" from to []

data IqStanza = MkIq { iqId   :: Text
                     , iqType :: Text
                     , iqContents :: [Event]  -- Change to a full XML document??
                     }
                deriving (Eq, Show)

baseIqHandler i t = do
  c <- void takeAnyTreeContent .| consume
  return $ MkIq i t c
