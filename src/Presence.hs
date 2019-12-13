{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module Presence where

import Conduit
import Database.Persist.Sqlite
import Data.Text (Text, unpack, pack, splitOn)
import GHC.Conc (atomically, forkIO, STM)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Control.Concurrent.STM.Map as STC
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import Data.XML.Types (Event(..), Content(..))
import qualified Data.Map as M

import Debug.Trace

import XMPP
import Users
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

presenceHandler :: (MonadThrow m,
                    MonadLogger m,
                    MonadReader XMPPSettings m,
                    MonadUnliftIO m) =>
  ChanMap -> Text -> Maybe Text -> ConduitT Event o m (Maybe ())
presenceHandler cm from t = do
  skipToEnd presenceName
  case (jidFromResource from, nameFromJid from) of
    (Just ownerJid, Just ownerName) -> do
      db <- asks xmppDB
      roster <- liftIO $ runSqlite db $ getRoster ownerName
      -- Send our presence to our roster.
      forM (rosterName <$> roster) (\jid -> sendToJidAll cm jid $ presenceElement from jid)
      -- Send the presence of our roster to us.
      if t /= Just "unavailable"
        then void $ forM (rosterName <$> roster)
             (\jid ->
                case nameFromJid jid of
                  Nothing -> return ()
                  Just name -> do
                    roster <- liftIO $ runSqlite db $ getRoster name
                    r <- liftIO . atomically $ do
                      mm <- STC.lookup jid cm
                      return $ void mm
                    case r of
                      Nothing -> return ()
                      Just _ -> sendToJidAll cm ownerJid $ presenceElement jid from)
        else return ()
      return $ Just ()
    _ -> return Nothing
  where
    presenceElement from to = Element presenceName
                         (M.union
                           (M.fromList [("from", from), ("to", to)])
                           (if t == Just "unavailable"
                            then M.fromList [("type", "unavailable")]
                            else M.empty)) []
