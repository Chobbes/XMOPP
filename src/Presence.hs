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

receivePresence :: MonadThrow m =>
     (Text -> ConduitT Event o m (Maybe c)) -> ConduitT Event o m (Maybe c)
receivePresence handler =
  join <$> tag' "{jabber:client}presence" attrs handler
  where
    attrs = requireAttr "from" <* ignoreAttrs

presenceHandler :: MonadThrow m => Text -> ConduitT Event o m (Maybe ())
presenceHandler from = do
  skipToEnd "{jabber:client}presence"
  return $ Just ()
