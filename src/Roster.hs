{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
module Roster where

import Database.Persist.Sqlite
import Data.Conduit
import Data.Conduit.Network
import Data.Text (Text, pack, unpack, splitOn)
import Data.XML.Types (Event(..), Content(..))
import GHC.Conc (atomically)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import qualified Control.Concurrent.STM.Map as STC
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.Map as M

import XMPP
import Stream
import Concurrency
import InternalMessaging
import Utils
import Users

nameFromJid :: JID -> Maybe Text
nameFromJid jid = case splitOn "@" jid of
                    [name, _] -> Just name
                    _ -> Nothing

jidFromResource :: XMPPResource -> Maybe JID
jidFromResource res = case splitOn "/" res of
                        [jid, _] -> Just jid
                        _ -> Nothing

rosterNamespace :: Text
rosterNamespace = "jabber:iq:roster"

rosterHandler :: (MonadThrow m, MonadLogger m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
  ChanMap ->
  ConduitT Element o m r ->
  Text ->
  Text ->
  JID ->
  ConduitT Event o m (Maybe r)
rosterHandler cm sink i t from =
  case nameFromJid from of
    Nothing -> do
      logDebugN $ "Roster error: invalid from attr: " <> from
      return Nothing
    Just owner -> join <$> tagNoAttr (matching (==rosterName)) (itemHandler cm owner)
  where
    rosterName = queryName rosterNamespace
    itemName = Name "item" (Just rosterNamespace) Nothing
    itemHandler cm owner = do
      db <- asks xmppDB
      fqdn <- asks fqdn
      case t of
        "get" -> do
            roster <- liftIO $ runSqlite db $ getRoster owner
            r <- yield (iq i "result" from fqdn
                        [NodeElement $ query rosterNamespace $ NodeElement <$> rosterItems roster]) .| sink
            return $ Just r
        "set" -> do
          attrs <- tag'
                   (matching (==itemName))
                   ((,) <$> requireAttr "jid" <*> attr "subscription" <* ignoreAttrs)
                   (\(jid, subscription) -> return (jid, subscription == Just "remove"))
          case attrs of
            Nothing -> return Nothing
            Just (jid, remove) -> do
              r <- liftIO $ runSqlite db $ if remove
                                           then removeRoster owner jid
                                           else addRoster owner jid
              case r of
                Nothing -> do
                  logDebugN $ "Roster error: user not found: " <> owner
                  return Nothing
                Just _ -> do
                  r <- yield (iq i "result" from fqdn []) .| sink
                  let ownerJid = owner <> "@" <> fqdn
                  if remove
                    then do -- Their roster may still contain us.
                    present <- isPresent cm jid
                    updatePresenceTo cm jid (not present) ownerJid
                    else -- Our roster will contain them.
                    updatePresenceTo cm ownerJid False jid
                  return $ Just r
        _ ->
          return Nothing

isPresent :: MonadIO m => ChanMap -> JID -> m Bool
isPresent cm jid = liftIO . atomically $ do
  mm <- STC.lookup jid cm
  case mm of
    Just (_, True, _) -> return True
    _ -> return False

rosterItems :: [Roster] -> [Element]
rosterItems = fmap (\roster -> Element
                               "item"
                               (M.fromList [("jid", rosterName roster)])
                               [])

getRoster :: (MonadIO m,
              PersistUniqueRead backend,
              PersistQueryRead backend,
              BaseBackend backend ~ SqlBackend) =>
  Text -> ReaderT backend m [Roster]
getRoster name = do
  userEntity <- getBy (UniqueName name)
  case userEntity of
    Nothing -> return []
    Just (Entity userId user) -> fmap entityVal <$> selectList [RosterOwner ==. userId] []

removeRoster :: (MonadIO m,
                 PersistUniqueRead backend,
                 PersistQueryWrite backend,
                 BaseBackend backend ~ SqlBackend) =>
     Text -> JID -> ReaderT backend m (Maybe Text)
removeRoster owner jid = do
  ownerEntity <- getBy (UniqueName owner)
  case ownerEntity of
    Nothing -> return Nothing
    Just (Entity ownerId _) -> do
      deleteWhere [RosterOwner ==. ownerId, RosterName ==. jid]
      return $ Just jid

-- Adds a new entry to the roster. TODO: Doesn't check for duplicates.
addRoster :: (MonadIO m,
              PersistUniqueRead backend,
              PersistStoreWrite backend,
              BaseBackend backend ~ SqlBackend) =>
  Text -> JID -> ReaderT backend m (Maybe Text)
addRoster owner jid = do
  ownerEntity <- getBy (UniqueName owner)
  case ownerEntity of
    Nothing -> return Nothing
    Just (Entity ownerId _) -> do
      insert (Roster ownerId jid)
      return $ Just jid

-- Exchange presence information between jid and jid'. jid is the user
-- whose presence information just changed.
updatePresenceTo :: (MonadReader XMPPSettings m, MonadIO m, MonadLogger m) =>
  ChanMap -> JID -> Bool -> JID -> m ()
updatePresenceTo cm jid offline jid' =
  case nameFromJid jid' of
    Nothing -> return ()
    Just name' -> do
      present <- isPresent cm jid'
      when present $ do
        db <- asks xmppDB
        roster' <- liftIO $ runSqlite db $ getRoster name'
        if jid `elem` (rosterName <$> roster')
          then do
          -- We're in their roster.
          -- Send new presence status.
          sendToJidAll cm jid' $ presenceElement jid jid' offline
          -- If we're just coming online, receive presence status from our roster.
          unless offline (sendToJidAll cm jid $ presenceElement jid' jid (not present))
          else do
          -- We're not in their roster. We should always look offline to each other.
          sendToJidAll cm jid' $ presenceElement jid jid' True
          sendToJidAll cm jid $ presenceElement jid' jid True
  where
    presenceElement from to offline = Element presenceName
                         (M.union
                           (M.fromList [("from", from), ("to", to)])
                           (if offline
                            then M.fromList [("type", "unavailable")]
                            else M.empty)) []
