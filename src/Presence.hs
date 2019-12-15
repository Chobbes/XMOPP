{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Presence where

import Conduit
import Data.Text
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..))

import XMPP
import Roster
import Utils

receivePresence :: MonadThrow m =>
     (Text -> Maybe Text -> ConduitT Event o m (Maybe c)) -> ConduitT Event o m (Maybe c)
receivePresence handler =
  join <$> tag' (matching (==presenceName)) attrs (uncurry handler)
  where
    attrs = (,) <$> requireAttr "from" <*> attr "type" <* ignoreAttrs

presenceHandler :: (MonadThrow m,
                    MonadLogger m,
                    MonadReader XMPPSettings m,
                    MonadUnliftIO m) =>
  ChanMap -> Text -> Maybe Text -> ConduitT Event o m (Maybe ())
presenceHandler cm from t = do
  skipToEnd presenceName -- We don't handle anything besides basic presence.
  case jidFromResource from of
    Nothing -> return $ Just ()
    Just jid -> updatePresence cm jid (t == Just "unavailable")
  return $ Just ()
