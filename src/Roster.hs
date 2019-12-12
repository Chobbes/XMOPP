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

import XMPP
import Stream
import Concurrency
import Utils
import Users

rosterNamespace :: Text
rosterNamespace = "jabber:iq:roster"

rosterHandler :: (MonadThrow m, MonadLogger m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
  ConduitT Element o m r ->
  Text ->
  Text ->
  Text ->
  ConduitT Event o m (Maybe r)
rosterHandler sink i t from = do
  q <- tagNoAttr (matching (==rosterName)) content
  case q of
    Nothing -> return Nothing
    Just _ ->
      case splitOn "@" from of
        [] -> do
          logDebugN $ "Roster error: invalid from attr: " <> from
          return Nothing
        (name:_) ->
          case t of
            "get" -> do
              db <- asks xmppDB
              do
                roster <- liftIO $ runSqlite db $ getRoster name
                r <- yield (iq i "result" from (fqdn def) $ [NodeElement $ query rosterNamespace $ rosterItems roster]) .| sink
                return $ Just r
            "set" -> do
              jid <- tag' "item" (requireAttr "jid") (\jid -> do
                                                         ignoreAnyTreeContent -- TODO
                                                         return jid)
              case jid of
                Nothing -> return Nothing
                Just jid -> do
                  db <- asks xmppDB
                  liftIO $ runSqlite db $ do
                    ownerEntity <- getBy (UniqueName name)
                    case ownerEntity of
                      Nothing -> logDebugN $ "Roster error: user not found: " <> name
                      Just (Entity ownerId _) -> do
                        void $ insert (Roster ownerId jid)
                  return Nothing
            _ -> return Nothing
  where
    rosterName = queryName rosterNamespace

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
