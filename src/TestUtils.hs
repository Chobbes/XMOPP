{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUtils where

import Data.Text as T (Text, unpack)
import Data.Text.Encoding

import Data.Default

import System.IO.Error
import Control.Exception

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Logger
import Control.Monad.Catch hiding (catch)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TMChan
import qualified Control.Concurrent.STM.Map as STC
import GHC.Conc (atomically, forkIO, STM)

import Data.Conduit
import Data.Conduit.TMChan
import Data.Conduit.List hiding (mapM_, concatMap)

import Database.Persist
import Database.Persist.Sqlite

import System.Directory

import Data.UUID
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, pack, append)
import qualified Data.ByteString.Base64 as BS64

import qualified Data.Map as M

import Data.XML.Types (Event(..), Content(..))
import qualified Data.XML.Types as XT
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Text.XML.Unresolved

import XMPP
import Users
import Concurrency
import SASL
import Stream
import Iq
import Utils

--------------------------------------------------
-- Test utilities / setup
--------------------------------------------------

-- | Default XMPPSettings for testing.
--
-- Sets things up to use an in memory database.
testSettings :: XMPPSettings
testSettings = def { xmppDB = "tests.db" }

-- | List of test users.
testUsers :: [User]
testUsers = [ testUser1
            , testUser2
            ]

testUser1 :: User
testUser1 = User "test1" "test1pass"

testUser2 :: User
testUser2 = User "test2" "test2pass"

testResources :: [[Text]]
testResources = [ ["u1r1", "u1r2"]
                , ["u2r1"]
                ]

testUserResources :: [(User, [Text])]
testUserResources = Prelude.zip testUsers testResources

testRoster :: [(User, User)]
testRoster = [(o, u) | o <- testUsers, u <- testUsers]

-- | Set up a bogus ChanMap with test users.
-- Given a list of (User, [Resource]) pairs.
testChanMapSetup :: (MonadIO m, MonadReader XMPPSettings m) => [(User, [Text])] -> m ChanMap
testChanMapSetup users = do
  dn <- asks fqdn
  m <- liftIO . atomically $ STC.empty
  forM_ users $ \(user, resources) ->
    forM_ resources $ \r ->
      allocateChannel m (userJid dn user) r
  return m

testChanMap :: (MonadIO m, MonadReader XMPPSettings m) => m ChanMap
testChanMap = testChanMapSetup testUserResources

-- | Set up DB with test users.
testDBSetup
  :: (MonadIO m, MonadReader XMPPSettings m) => m ()
testDBSetup = do
  db <- asks xmppDB
  liftIO . runSqlite db $ insertUsers testUsers

-- | Remove a file unconditionally.
removeFileUncond :: FilePath -> IO ()
removeFileUncond name = removeFile name `catch` handleExcept
  where handleExcept e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

-- | TODO: unfinished
runWithTestDB :: (MonadIO m, MonadReader XMPPSettings m) => m ()
runWithTestDB = do
  db <- asks xmppDB
  liftIO $ runSqlite db $ runMigration migrateAll

-- | DELETE the DB and run with a fresh one.
cleanRunWithTestDB :: (MonadIO m, MonadReader XMPPSettings m) => m ()
cleanRunWithTestDB = do
  db <- asks xmppDB
  liftIO $ removeFileUncond (unpack db)
  runWithTestDB
  testDBSetup

-- | Create a test sink from a TMVar [i].
testSink :: MonadIO m => TMVar [i] -> ConduitT i o m ()
testSink tv = do
  e <- consume
  liftIO $ atomically $ do
    e' <- tryTakeTMVar tv
    case e' of
      Nothing -> putTMVar tv e
      Just e' -> putTMVar tv (e' ++ e)

-- | Create a sink for testing.
newTestSink :: (MonadIO m, MonadIO mc) => m (ConduitT i o mc (), TMVar [i])
newTestSink = do
  tv <- liftIO newEmptyTMVarIO
  return (testSink tv, tv)

runTestConduit
  :: ConduitT () Void (ReaderT XMPPSettings (NoLoggingT IO)) a -> IO a
runTestConduit = liftIO . runNoLoggingT . flip runReaderT testSettings . runConduit

--------------------------------------------------
-- Generating XMPP stanzas
--------------------------------------------------

-- | Create an XML message for login.
createAuthStanza :: User -> Element
createAuthStanza (User user pass) =
  Element authName
          (M.fromList [("mechanism","PLAIN")])
          [ NodeContent authText ]
  where authText = decodeUtf8 . BS64.encode $ mconcat [BS.singleton 0, userBS, BS.singleton 0, passBS]
        userBS = encodeUtf8 user
        passBS = encodeUtf8 pass

-- | Create a message with UUID.
createMessage :: JID -> JID -> Text -> UUID -> Element
createMessage to from body uuid =
  Element "{jabber:client}message"
          (M.fromList [ ("from",from)
                      , ("to",to)
                      , ("type","chat")
                      , ("id", toText uuid)])
          [ NodeElement (Element "{jabber:client}body" mempty [NodeContent body])
          , NodeElement (Element "{urn:xmpp:sid:0}origin-id" (M.fromList [("id", toText uuid)]) [])
          , NodeElement (Element "{urn:xmpp:receipts}request" mempty [])
          , NodeElement (Element "{jabber:client}thread" mempty [NodeContent "testThreadId"])
          ]

-- | Open a stream.
createOpenStream :: Text -> Event
createOpenStream fqdn =
  EventBeginElement streamName [("to", [ContentText fqdn])]

-- | Create iq element.
createIq :: Text -> UUID -> [Node] -> Element
createIq t uuid =
  Element "iq"
          (M.fromList [ ("type", t)
                      , ("id", toText uuid)
                      ])

-- | Create element for bind
createBind :: XMPPResource -> UUID -> Element
createBind resource uuid = createIq "set" uuid [NodeElement bindElem]
  where
    bindElem     = Element bindName mempty [NodeElement resourceElem]
    resourceElem = Element "{urn:ietf:params:xml:ns:xmpp-bind}resource" mempty [NodeContent resource]


-- | Create a source out of a list of elements.
elementsToSource
  :: (Monad m, Foldable t) => t Element -> ConduitT i Event m ()
elementsToSource elems =
  sourceList $ concatMap (elementToEvents . toXMLElement) elems
