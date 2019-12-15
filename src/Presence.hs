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
import Database.Persist.Sqlite
import GHC.Conc (atomically)
import qualified Control.Concurrent.STM.Map as STC

import Users
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
  skipToEnd -- We don't handle anything besides basic presence.
  case jidFromResource from of
    Nothing -> return $ Just ()
    Just jid -> updatePresence cm jid (t == Just "unavailable")
  return $ Just ()

-- Handles the case when the presence of a resource has changed.
updatePresence :: (MonadReader XMPPSettings m, MonadIO m, MonadLogger m) =>
  ChanMap -> JID -> Bool -> m (Maybe ())
updatePresence cm jid offline =
  case nameFromJid jid of
    Just name -> do
      -- Update the flag in the ChanMap.
      liftIO . atomically $ do
        mm <- STC.lookup jid cm
        case mm of
          Just (xs, _, m) -> STC.insert jid (xs, not offline, m) cm
          _ -> return ()
      -- Share presence with other users.
      db <- asks xmppDB
      roster <- liftIO $ runSqlite db $ getRoster name
      forM_ (rosterName <$> roster) $ updatePresenceTo cm jid offline
      return $ Just ()
    _ -> return Nothing
