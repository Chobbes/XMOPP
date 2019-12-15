{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Tests where

import Test.HUnit
import Control.Monad.ST
import Text.XML.Stream.Render
import Data.Maybe
import Data.UUID
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, pack, append)
import Data.Conduit
import Data.Default
import Data.Conduit.List hiding (mapM_)
import Data.Text as T (Text, unpack)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TMChan
import qualified Control.Concurrent.STM.Map as STC
import Control.Monad
import Control.Monad.STM
import Control.Monad.Catch hiding (catch)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Logger
import qualified Data.Map as M
import Data.XML.Types (Event(..), Content(..))
import qualified Data.XML.Types as XT
import Text.XML hiding (parseText)
import Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render as XR
import Text.XML.Unresolved
import GHC.Conc (atomically, forkIO, STM)
import Data.Conduit.TMChan
import System.Random
import Database.Persist
import Database.Persist.Sqlite
import System.Directory
import Control.Exception
import System.IO.Error
import Control.Concurrent.STM.Map hiding (insert)

import Main hiding (main)
import XMPP
import XMLRender
import Users
import Stream
import TLS
import SASL
import Iq
import Concurrency
import Logging
import Messages
import InternalMessaging
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
  :: (MonadIO m, MonadReader XMPPSettings m) => Text -> m ()
testDBSetup db = do
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

testSink :: MonadIO m => TMVar [i] -> ConduitT i o m ()
testSink tv = do
  e <- consume
  liftIO $ atomically $ do
    e' <- tryTakeTMVar tv
    case e' of
      Nothing -> putTMVar tv e
      Just e' -> putTMVar tv (e' ++ e)

runTestConduit
  :: ConduitT () Void (ReaderT XMPPSettings (NoLoggingT IO)) a -> IO a
runTestConduit = liftIO . runNoLoggingT . flip runReaderT testSettings . runConduit


--------------------------------------------------
-- Test messaging
--------------------------------------------------
testmsg :: BS.ByteString
testmsg = "<message xmlns=\"jabber:client\" from=\"test@localhost/gajim.CD9NEZ09\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" to=\"foo@localhost\" type=\"chat\"><body>eoaueoa</body><origin-id xmlns=\"urn:xmpp:sid:0\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" /><request xmlns=\"urn:xmpp:receipts\" /><thread>MrwqjWfrzhgjYOPHfuQwOjgWuSTHWIcM</thread></message>"

testmsgbroken :: BS.ByteString
testmsgbroken = "<messag xmlns=\"jabber:client\" from=\"test@localhost/gajim.CD9NEZ09\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" to=\"foo@localhost\" type=\"chat\"><body>eoaueoa</body><origin-id xmlns=\"urn:xmpp:sid:0\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" /><request xmlns=\"urn:xmpp:receipts\" /><thread>MrwqjWfrzhgjYOPHfuQwOjgWuSTHWIcM</thread></message>"

testmsgElem :: Element
testmsgElem = createMessage "foo@localhost"
              "test@localhost/gajim.CD9NEZ09"
              "blah"
              (fromJust (fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"))

-- | Test that the receive message function only parses messages.
testReceiveMessage :: Test
testReceiveMessage = TestList
  [ receiveGood ~?= Just testmsgElem
  , receiveBad  ~?= Nothing
  , receiveBad2 ~?= Nothing
  ]
  where
    receiveGood = aux $ renderElement testmsgElem
    receiveBad  = aux testmsgbroken
    receiveBad2 = aux "<a></a>"

    aux msg = join . join $
      runConduit (yield msg .| parseBytes def .| receiveMessage messageBody)

-- | Create a message with UUID.
createMessage :: Text -> Text -> Text -> UUID -> Element
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

-- | Test sending message from one user to another.
testMessaging :: (MonadIO m, MonadThrow m, MonadReader XMPPSettings m, MonadLogger m) =>
  User -> User -> m Bool
testMessaging toUser fromUser = do
  cm <- testChanMap
  dn <- asks fqdn

  let to   = userJid dn toUser
  let from = userJid dn fromUser

  uuid <- liftIO randomIO

  let msg = createMessage to from "yohoo" uuid
  runConduit $ sourceList (elementToEvents (toXMLElement msg)) .| receiveMessage (messageHandler cm)

  -- Fetch message
  channels <- liftIO . atomically $ getJidChannels cm (userJid dn toUser)
  elems <- liftIO . atomically $ readAllChannels channels

  -- Make sure message matches what we sent
  return $ Prelude.all (==Just msg) elems

-- | Fetch all (maybe) messages for a user.
lookupUserMessages :: ChanMap -> User -> STM [Maybe Element]
lookupUserMessages cm user = do
  channels <- getJidChannels cm (userName user)
  readAllChannels channels

-- Utils tests

test_iq :: Test
test_iq = TestList
  [ renderElement (iq "id" "type" "to" "from" []) ~?=
    "<iq from=\"from\" id=\"id\" to=\"to\" type=\"type\" xmlns=\"jabber:client\"/>"
  , renderElement (iq "i" "t" "t" "f" $ NodeElement <$> [proceed, proceed]) ~?=
    "<iq from=\"f\" id=\"i\" to=\"t\" type=\"t\" xmlns=\"jabber:client\"><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/></iq>" ]

test_query :: Test
test_query = TestList
  [ renderElement (query "info" []) ~?=
    "<query xmlns=\"info\"/>"
  , renderElement (query "items" $ NodeElement <$> [proceed, proceed]) ~?=
    "<query xmlns=\"items\"><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/></query>" ]

test_skipToEnd :: Test
test_skipToEnd = TestList
  [ runST (runConduit $ yield "test</a>" .| parseBytes def .| (skipToEnd >> consume)) ~?=
    [EventEndElement "a", EventEndDocument]
  , runST (runConduit $ yield "<a>test</a></a>" .| parseBytes def .| (skipToEnd >> consume)) ~?=
    [EventEndElement "a", EventEndDocument]
  , runST (runConduit $ yield "<a>test</a></a><asdf/>" .| parseBytes def .| (skipToEnd >> consume)) ~?=
    [EventEndElement "a", EventBeginElement "asdf" [], EventEndElement "asdf", EventEndDocument] ]

-- Stream module tests

test_awaitName :: Test
test_awaitName = TestList
  [ join (runConduit $ yield "<stream/>" .| parseBytes def .| awaitName "stream") ~?=
    Just (EventBeginElement "stream" [])
  , join (runConduit $ yield "<notstream/>" .| parseBytes def .| awaitName "stream") ~?=
    Nothing
  , join (runConduit $ yield "<notstream/><stream/>" .| parseBytes def .| awaitName "stream") ~?=
    Just (EventBeginElement "stream" []) ]

test_streamRespHeader :: Test
test_streamRespHeader =
  runST (runConduit $
          streamRespHeader "localhost" "en"
          (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") .|
          XR.renderBytes def .|
          consume) ~?=
  ["<stream:stream from=\"localhost\" version=\"1.0\" id=\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_features :: Test
test_features = TestList
  [ renderElement (features []) ~?=
    "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"/>"
  , renderElement (features [NodeElement required]) ~?=
    "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><required/></stream:features>" ]

test_required :: Test
test_required = renderElement required ~?= "<required/>"

-- TLS tests

test_proceed :: Test
test_proceed = renderElement proceed ~?=
               "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>"

-- Has a few extra namespaces but seems to work with clients
test_tlsFeatures :: Test
test_tlsFeatures = renderElement tlsFeatures ~?=
                   "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required xmlns=\"\"/></starttls></stream:features>"

-- SASL tests

-- TODO check PLAIN
test_awaitAuth :: Test
test_awaitAuth = TestList
  [ join (runConduit $ yield "<auth xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\" mechanism=\"PLAIN\">AGdyYWluAGFzZGY=</auth>" .| parseBytes def .| awaitAuth) ~?=
    Just ("grain", "asdf")
  , join (runConduit $ yield "<notauth/>" .| parseBytes def .| awaitAuth) ~?=
    Nothing ]

test_failure :: Test
test_failure = TestList
  [ renderElement (failure []) ~?=
    "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"
  , renderElement (failure $ NodeElement <$> [proceed, proceed]) ~?=
    "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/></failure>" ]

test_notAuthorized :: Test
test_notAuthorized = renderElement notAuthorized ~?=
                     "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><not-authorized xmlns=\"\"/></failure>"

test_success :: Test
test_success = renderElement success ~?=
               "<success xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"

test_authFeatures :: Test
test_authFeatures = renderElement authFeatures ~?=
                    "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><mechanism xmlns=\"\">PLAIN</mechanism></mechanisms></stream:features>"

-- Iq tests

test_bindFeatures :: Test
test_bindFeatures = renderElement bindFeatures ~?=
                    "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"/></stream:features>"

test_iqShort :: Test
test_iqShort = TestList
  [ renderElement (iqShort "id" "type" []) ~?=
    "<iq id=\"id\" type=\"type\" xmlns=\"jabber:client\"/>"
  , renderElement (iqShort "i" "t" $ NodeElement <$> [proceed, proceed]) ~?=
    "<iq id=\"i\" type=\"t\" xmlns=\"jabber:client\"><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/><proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/></iq>" ]

test_bind :: Test
test_bind = renderElement (bind "test") ~?=
            "<bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test</jid></bind>"

test_identity :: Test
test_identity = TestList
  [ renderElement (identity "c" "t" "n") ~?=
    "<identity category=\"c\" name=\"n\" type=\"t\"/>"
  , renderElement (identity "" "" "") ~?=
    "<identity category=\"\" name=\"\" type=\"\"/>" ]

test_feature :: Test
test_feature = TestList
  [ renderElement (feature "f") ~?=
    "<feature var=\"f\"/>"
  , renderElement (feature "") ~?=
    "<feature var=\"\"/>" ]

-- test_login_success :: Test
-- test_login_success = undefined

-- test_login_fail :: Test
-- test_login_fail = undefined

----------------------------
-- Tests that require IO
----------------------------

-- Stream tests
test_openStream :: IO Bool
test_openStream = do
  tv <- newEmptyTMVarIO :: IO (TMVar [ByteString])
  let sink = testSink tv

  uuid <- randomIO
  runTestConduit $ openStream uuid (yield "<anything/>" .| parseBytes def) sink
  sent <- atomically $ tryTakeTMVar tv

  return $ sent ==
    Just [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_initiateStream :: IO Bool
test_initiateStream = do
  tv <- newEmptyTMVarIO :: IO (TMVar [ByteString])
  let sink = testSink tv

  uuid <- randomIO
  runTestConduit $ initiateStream uuid sink
  sent <- atomically $ tryTakeTMVar tv

  return $ sent ==
    Just [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

--------------------------------------------------
-- TLS tests
--------------------------------------------------

test_startTLS :: IO Bool
test_startTLS = do
  tv <- newEmptyTMVarIO :: IO (TMVar [Element])
  let sink = testSink tv

  runTestConduit $ startTLS (yield "<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>" .| parseBytes def) sink
  sent <- atomically $ tryTakeTMVar tv

  return $ (fmap renderElement <$> sent) ==
    Just [ "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required xmlns=\"\"/></starttls></stream:features>"
     , "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>" ]

--------------------------------------------------
-- SASL tests
--------------------------------------------------

test_authenticate1 :: IO Bool
test_authenticate1 = liftIO $ runSqlite ":memory:" $ do
  runMigrationSilent migrateAll
  insert $ User "grain" "asdf"
  u <- authenticate "grain" "asdf"
  return $
    u == Just (User "grain" "asdf")

test_authenticate2 :: IO Bool
test_authenticate2 = liftIO $ runSqlite ":memory:" $ do
  runMigrationSilent migrateAll
  insert $ User "grain" "1234"
  u <- authenticate "grain" "asdf"
  return $
    u == Nothing

--------------------------------------------------
-- Iq bind tests
--------------------------------------------------

testIqInfo :: ByteString
testIqInfo = "<iq type='get' from='romeo@montague.net/orchard' to='plays.shakespeare.lit' id='info1'> <query xmlns='http://jabber.org/protocol/disco#info'/> </iq>"


--------------------------------------------------
-- Testing bind handler
--------------------------------------------------

test_bindHandler1 :: IO Bool
test_bindHandler1 = do
  tv <- newEmptyTMVarIO :: IO (TMVar [Element])
  let sink = testSink tv

  cm <- atomically STC.empty
  -- not an iq stanza
  r <- runTestConduit $ yield "<asdf/>" .| parseBytes def .| receiveIqBind (bindHandler cm "test@localhost" sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_bindHandler2 :: IO Bool
test_bindHandler2 = do
  tv <- newEmptyTMVarIO :: IO (TMVar [Element])
  let sink = testSink tv

  cm <- atomically STC.empty
  -- not of type set
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"get\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>resourceid</resource></bind></iq>" .| parseBytes def .| receiveIqBind (bindHandler cm "test@localhost" sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_bindHandler3 :: IO Bool
test_bindHandler3 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>resourceid</resource></bind></iq>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@localhost" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just "resourceid" &&
    (fmap renderElement <$> sent) == Just ["<iq id=\"id\" type=\"result\" xmlns=\"jabber:client\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test@localhost/resourceid</jid></bind></iq>"]

test_bindHandler4 :: IO Bool
test_bindHandler4 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"/></iq>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@localhost" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ (fmap renderElement <$> sent) ==
    Just ["<iq id=\"id\" type=\"result\" xmlns=\"jabber:client\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test@localhost/" <> pack (maybe "" T.unpack r) <> "</jid></bind></iq>"]

--------------------------------------------------
-- Other Iq tests
--------------------------------------------------

-- wrap the handlers that require a to field to give a default one
handlerWrapper :: MonadThrow m =>
  (Text -> Text -> Text -> Text -> ConduitT Event o m (Maybe r)) ->
  (Text -> Text -> Text -> Maybe Text -> ConduitT Event o m (Maybe r))
handlerWrapper handler a b c Nothing = handler a b c "default"
handlerWrapper handler a b c (Just d) = handler a b c d

-- info

test_infoHandler1 :: IO Bool
test_infoHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . handlerWrapper $ infoHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler2 :: IO Bool
test_infoHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq . handlerWrapper $ infoHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"cat\" name=\"name\" type=\"type\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#items\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/><feature var=\"jabber:iq:roster\" xmlns=\"\"/></query></iq>"]

-- items

test_itemsHandler1 :: IO Bool
test_itemsHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . handlerWrapper $ itemsHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler2 :: IO Bool
test_itemsHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq . handlerWrapper $ itemsHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>"]

--------------------------------------------------
-- ping
--------------------------------------------------

test_pingHandler1 :: IO Bool
test_pingHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing ping
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . handlerWrapper $ pingHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_pingHandler2 :: IO Bool
test_pingHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq . handlerWrapper $ pingHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"/>"]

--------------------------------------------------
-- errors
--------------------------------------------------

test_iqError :: IO Bool
test_iqError = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . handlerWrapper $ iqError sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

-- combined iq handler

test_iqHandler1 :: IO Bool
test_iqHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"cat\" name=\"name\" type=\"type\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#items\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/><feature var=\"jabber:iq:roster\" xmlns=\"\"/></query></iq>"]

test_iqHandler2 :: IO Bool
test_iqHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>"]

test_iqHandler3 :: IO Bool
test_iqHandler3 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"/>"]

test_iqHandler4 :: IO Bool
test_iqHandler4 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

main :: IO ()
main = do
  runTestTT unitTests
  runTests ioTests
  where
    ioTests = [ (test_initiateStream, "initiateStream")
              , (test_openStream, "openStream")
              , (test_startTLS, "startTLS")
              , (test_authenticate1, "authenticate 1")
              , (test_authenticate2, "authenticate 2")
              , (test_bindHandler1, "bindHandler 1")
              , (test_bindHandler2, "bindHandler 2")
              , (test_bindHandler3, "bindHandler 3")
              , (test_bindHandler4, "bindHandler 4")
              , (test_infoHandler1, "infoHandler 1")
              , (test_infoHandler2, "infoHandler 2")
              , (test_itemsHandler1, "itemsHandler 1")
              , (test_itemsHandler2, "itemsHandler 2")
              , (test_pingHandler1, "pingHandler 1")
              , (test_pingHandler2, "pingHandler 2")
              , (test_iqError, "iqError")
              , (test_iqHandler1, "iqHandler 1")
              , (test_iqHandler2, "iqHandler 2")
              , (test_iqHandler3, "iqHandler 3")
              , (test_iqHandler4, "iqHandler 4")
              , (runXMPP $ testMessaging testUser1 testUser2, "Test Messages from 1 to 2")
              , (runXMPP $ testMessaging testUser1 testUser2, "Test Messages from 2 to 1")]

    unitTests :: Test
    unitTests = TestList
      [ "iq"               ~: test_iq
      , "query"            ~: test_query
      , "skipToEnd"        ~: test_skipToEnd
      , "awaitName"        ~: test_awaitName
      , "streamRespHeader" ~: test_streamRespHeader
      , "features"         ~: test_features
      , "required"         ~: test_required
      , "proceed"          ~: test_proceed
      , "tlsFeatures"      ~: test_tlsFeatures
      , "awaitAuth"        ~: test_awaitAuth
      , "failure"          ~: test_failure
      , "notAuthorized"    ~: test_notAuthorized
      , "success"          ~: test_success
      , "authFeatures"     ~: test_authFeatures
      , "bindFeatures"     ~: test_bindFeatures
      , "iqShort"          ~: test_iqShort
      , "bind"             ~: test_bind
      , "identity"         ~: test_identity
      , "feature"          ~: test_feature
      , "receiveMessage"   ~: testReceiveMessage
      ]


    runXMPP = runNoLoggingT . flip runReaderT testSettings

    runTests [] = return ()
    runTests ((t, name):ts) = do
      result <- t
      print $ name ++ (if result then " passed" else " failed *******************")
      runTests ts

--------------------------------------------------
-- Misc test stuff
--------------------------------------------------

testiq :: ByteString
testiq = "<iq id=\"5ba62e81-cbbd-45cc-a20a-5abca191b55f\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>gajim.CD9NEZ09</resource></bind></iq>"

eventConduitTest sink = do
  chan <- liftIO newTMChanIO
--  streamId <- liftIO $ randomIO

  let tmSource = sourceTMChan chan
  let tmSink   = sinkTMChan chan
--  let docond = do v <- await; yield (fromJust v); liftIO $ print "test"; liftIO $ print v; docond

  forkIO (runConduit $ tmSource .| renderElements .| sink)
--  forkIO $ chanToSink chan sink
  forkIO (runConduit $ yield testElement .| tmSink)


testElement = Element "message" (M.fromList [("to", "foo"), ("from", "calvin")]) []
testDocument = Document emptyPrologue testElement []
