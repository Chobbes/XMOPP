{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Presence where

import Conduit
import Data.Text (Text, unpack, splitOn)
import Control.Monad
import Control.Monad.Logger
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))
import qualified Data.Map as M

import Debug.Trace

import XMPP
import InternalMessaging
import Roster
import Utils

presenceName :: Name
presenceName = Name {nameLocalName = "presence", nameNamespace = Just "jabber:client", namePrefix = Nothing}

receivePresence :: MonadThrow m =>
     (Text -> Maybe Text -> ConduitT Event o m (Maybe c)) -> ConduitT Event o m (Maybe c)
receivePresence handler =
  join <$> tag' (matching (==presenceName)) attrs (uncurry handler)
  where
    attrs = (,) <$> requireAttr "from" <*> attr "type" <* ignoreAttrs

jidFromResource :: Text -> Maybe JID
jidFromResource res = case splitOn "/" res of
                        [] -> Nothing
                        (jid:_) -> Just jid

presenceHandler :: (MonadThrow m, MonadIO m, MonadLogger m) =>
  ChanMap -> Text -> Maybe Text -> ConduitT Event o m (Maybe ())
presenceHandler cm from t = do
  skipToEnd presenceName
  case jidFromResource from of
    Nothing -> return Nothing
    Just jid -> do
      sendToJidAll cm jid $ Element "test" M.empty []
      return $ Just ()
