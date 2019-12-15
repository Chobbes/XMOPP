{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
module SASL where

import Data.Conduit
import Data.Conduit.Network
import Data.Text (Text)
import Data.Text.Encoding
import Data.Default
import Data.ByteString.Base64 (decodeLenient)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Primitive
import Control.Monad.IO.Unlift
import Database.Persist
import Database.Persist.Sqlite
import Data.XML.Types (Event(..), Content(..))
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Data.ByteString as BS

import Users
import XMPP
import Stream
import Concurrency

saslNamespace :: Text
saslNamespace = "urn:ietf:params:xml:ns:xmpp-sasl"

awaitAuth :: MonadThrow m => ConduitT Event o m (Maybe (Text, Text))
awaitAuth = do
  authStr <- tagIgnoreAttrs (matching (==(Name {nameLocalName = "auth", nameNamespace = Just saslNamespace, namePrefix = Nothing}))) content -- TODO check for mechanism='PLAIN'
  return $ do
    auth <- authStr
    case decodeUtf8 <$> (BS.split 0 . decodeLenient $ encodeUtf8 auth) of
      [_, user, pass] -> return (user, pass)
      _               -> Nothing


-- | Get authentication information.
plainAuth
  :: (PrimMonad m, MonadThrow m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
     ConduitT i Event m ()
     -> ConduitT Element o m r
     -> ConduitT i o m (Maybe User)
plainAuth source sink = do
  yield authFeatures .| sink
  auth <- source .| awaitAuth
  case auth of
    Just (user, pass) -> do
      db <- asks xmppDB -- TODO move db to an argument for testing?
      liftIO $ runSqlite db $ authenticate user pass
    _ -> return Nothing

authenticate :: (MonadIO m, PersistUniqueRead backend, BaseBackend backend ~ SqlBackend) =>
  Text -> Text -> ReaderT backend m (Maybe User)
authenticate user pass = do
  maybeUser <- getBy (UniqueName user)
  return $
    case maybeUser of
      Just (Entity _ u) ->
        if userPassword u == pass then Just u else Nothing
      _ -> Nothing

failure :: [Node] -> Element
failure = Element (Name "failure" (Just saslNamespace) Nothing) mempty

notAuthorized :: Element
notAuthorized = failure [NodeElement notAuthElement]
  where notAuthElement = Element "not-authorized" mempty []

success :: Element
success = Element (Name "success" (Just saslNamespace) Nothing) mempty []

authFeatures :: Element
authFeatures = features [NodeElement mechanisms]
  where
    mechanisms = Element mechanismsName mempty [NodeElement plain]
    mechanismsName = Name "mechanisms"
                          (Just saslNamespace)
                          Nothing
    plain = Element "mechanism" mempty [NodeContent "PLAIN"]
