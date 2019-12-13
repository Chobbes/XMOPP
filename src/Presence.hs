{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Presence where

import Conduit
import Data.Text (Text, unpack)
import Control.Monad
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))
import qualified Data.Map as M

import Debug.Trace

import XMPP
import Utils

presenceName :: Name
presenceName = Name {nameLocalName = "presence", nameNamespace = Just "jabber:client", namePrefix = Nothing}

receivePresence :: MonadThrow m =>
     (Text -> Maybe Text -> ConduitT Event o m (Maybe c)) -> ConduitT Event o m (Maybe c)
receivePresence handler =
  join <$> tag' (matching (==presenceName)) attrs (uncurry handler)
  where
    attrs = (,) <$> requireAttr "from" <*> attr "type" <* ignoreAttrs

presenceHandler :: MonadThrow m =>
  ChanMap -> Text -> Maybe Text -> ConduitT Event o m (Maybe ())
presenceHandler cm from t = do
  skipToEnd presenceName
  return $ Just ()
