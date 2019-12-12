{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
module Roster where

import Database.Persist
import Database.Persist.Sqlite
import Data.Conduit
import Data.Conduit.List
import Data.Conduit.Network
import Data.Text (Text, pack, unpack, splitOn)
import Data.XML.Types (Event(..), Content(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.Map as M

import Debug.Trace

import XMPP
import Stream
import Concurrency
import Utils
import Users

nameFromJid :: Text -> Maybe Text
nameFromJid jid = case splitOn "@" jid of
                    [] -> Nothing
                    (name:_) -> Just name

rosterNamespace :: Text
rosterNamespace = "jabber:iq:roster"

rosterHandler :: (MonadThrow m, MonadLogger m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
rosterHandler sink i t from = do
  join <$> tagNoAttr (matching (==rosterName)) itemHandler
  where
    rosterName = queryName rosterNamespace
    itemName = Name "item" (Just rosterNamespace) Nothing
    itemHandler =
      case nameFromJid from of
        Nothing -> do
          logDebugN $ "Roster error: invalid from attr: " <> from
          return Nothing
        Just name ->
          case t of
            "get" -> do
              db <- asks xmppDB
              do
                roster <- liftIO $ runSqlite db $ getRoster name
                r <- yield (iq i "result" from (fqdn def) $ [NodeElement $ query rosterNamespace $ rosterItems roster]) .| sink
                return $ Just r
            "set" -> do
              jid <- tag' (matching (==itemName)) (requireAttr "jid") return
              case join (nameFromJid <$> jid) of
                Nothing -> return Nothing
                Just jid -> do
                  db <- asks xmppDB
                  r <- liftIO $ runSqlite db $ addRoster name jid
                  case r of
                    Nothing -> do
                      logDebugN $ "Roster error: user not found: " <> name
                      return Nothing
                    Just _ -> do
                      r <- yield (iq i "result" from (fqdn def) $ []) .| sink
                      return $ Just r
            _ ->
              return Nothing

rosterItems :: [Entity Roster] -> [Node]
rosterItems l = fmap (\(Entity _ roster) ->
                         NodeElement $ Element
                         "item"
                         (M.fromList [("jid", rosterName roster <> "@" <> fqdn def)])
                         []) l

getRoster :: (MonadIO m,
              PersistUniqueRead backend,
              PersistQueryRead backend,
              BaseBackend backend ~ SqlBackend) =>
  Text -> ReaderT backend m [Entity Roster]
getRoster name = do
  userEntity <- getBy (UniqueName name)
  case userEntity of
    Nothing -> return []
    Just (Entity userId user) -> selectList [RosterOwner ==. userId] []

-- Adds a new entry to the roster. TODO: Doesn't check for duplicates.
addRoster :: (MonadIO m,
              PersistUniqueRead backend,
              PersistStoreWrite backend,
              BaseBackend backend ~ SqlBackend) =>
     Text -> Text -> ReaderT backend m (Maybe Text)
addRoster name jid = do
  ownerEntity <- getBy (UniqueName name)
  case ownerEntity of
    Nothing -> return Nothing
    Just (Entity ownerId _) -> do
      void $ insert (Roster ownerId jid)
      return $ Just jid

-- Temp testing for inserting things into roster since I can't figure out how to do it in sqlite
main :: IO ()
main = runSqlite (xmppDB def) $ do
  userEntity <- getBy (UniqueName "grain")
  case userEntity of
    Nothing -> return ()
    Just (Entity userId user) -> do
      insert $ Roster userId "paul"
      roster <- selectList [RosterOwner ==. userId] []
      liftIO $ print roster
