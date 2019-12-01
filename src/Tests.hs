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
import Data.ByteString.Char8
import Data.Conduit
import Data.Default
import Data.Conduit.List hiding (mapM_)
import Data.Text (Text, unpack)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TMChan
import qualified Control.Concurrent.STM.Map as STC
import Control.Monad
import Control.Monad.STM
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
import GHC.Conc (atomically, forkIO, STM)
import Data.Conduit.TMChan
import System.Random
import Database.Persist
import Database.Persist.Sqlite

import Database.Persist
import Database.Persist.Sqlite

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


--------------------------------------------------
-- Test utilities / setup
--------------------------------------------------

-- | Default XMPPSettings for testing.
--
-- Sets things up to use an in memory database.
testSettings :: XMPPSettings
testSettings = def { xmppDB = ":memory:" }

-- | List of test users.
testUsers :: [User]
testUsers = [ User "test1" "test1pass"
            , User "test2" "test2pass"
            ]

-- | Set up DB with test users.
testDBSetup
  :: (MonadIO m, MonadReader XMPPSettings m) => Text -> m ()
testDBSetup db = do
  db <- asks xmppDB
  liftIO . runSqlite db $ insertUsers testUsers

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
test_required = renderElement required ~?=
                "<required/>"

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

test_aborted :: Test
test_aborted = renderElement aborted ~?=
               "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><aborted xmlns=\"\"/></failure>"

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

unitTests :: Test
unitTests = TestList
  [ "awaitName" ~: test_awaitName
  , "streamRespHeader" ~: test_streamRespHeader
  , "features" ~: test_features
  , "required" ~: test_required
  , "proceed"  ~: test_proceed
  , "tlsFeatures" ~: test_tlsFeatures
  , "awaitAuth" ~: test_awaitAuth
  , "failure" ~: test_failure
  , "aborted" ~: test_aborted
  , "notAuthorized" ~: test_notAuthorized
  , "success"  ~: test_success
  , "authFeatures" ~: test_authFeatures
  , "bindFeatures" ~: test_bindFeatures
  , "iqShort" ~: test_iqShort
  , "bind" ~: test_bind
  , "iq" ~: test_iq
  , "query" ~: test_query
  , "identity" ~: test_identity
  , "feature" ~: test_feature ]

-- Tests that require IO

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
runTestConduit = liftIO . runNoLoggingT . flip runReaderT def . runConduit

-- Stream tests
-- TODO tests for whether the client initiates or we initiate. See comments in Stream.hs for details.
test_openStream :: IO Bool
test_openStream = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [ByteString]))
  let sink = testSink tv

  uuid <- randomIO
  runTestConduit $ openStream uuid (yield "<anything/>" .| parseBytes def) sink
  sent <- atomically $ tryTakeTMVar tv

  return $ sent ==
    Just [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_initiateStream :: IO Bool
test_initiateStream = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [ByteString]))
  let sink = testSink tv

  uuid <- randomIO
  runTestConduit $ initiateStream uuid sink
  sent <- atomically $ tryTakeTMVar tv

  return $ sent ==
    Just [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

-- TLS tests

test_startTLS :: IO Bool
test_startTLS = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  runTestConduit $ startTLS (yield "<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>" .| parseBytes def) sink
  sent <- atomically $ tryTakeTMVar tv

  return $ (fmap renderElement <$> sent) ==
    Just [ "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required xmlns=\"\"/></starttls></stream:features>"
     , "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>" ]

-- SASL tests

test_authenticate1 :: MonadIO m => m Bool
test_authenticate1 = liftIO $ runSqlite ":memory:" $ do
  runMigrationSilent migrateAll
  insert $ User "grain" "asdf"
  u <- authenticate "grain" "asdf"
  return $
    u == (Just $ User "grain" "asdf")

test_authenticate2 :: MonadIO m => m Bool
test_authenticate2 = liftIO $ runSqlite ":memory:" $ do
  runMigrationSilent migrateAll
  insert $ User "grain" "1234"
  u <- authenticate "grain" "asdf"
  return $
    u == Nothing

-- Iq bind tests

testIqInfo :: ByteString
testIqInfo = "<iq type='get' from='romeo@montague.net/orchard' to='plays.shakespeare.lit' id='info1'> <query xmlns='http://jabber.org/protocol/disco#info'/> </iq>"

test_bindHandler1 :: IO Bool
test_bindHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  -- not an iq stanza
  r <- runTestConduit $ yield "<asdf/>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@localhost" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_bindHandler2 :: IO Bool
test_bindHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  -- not of type set
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"get\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>resourceid</resource></bind></iq>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@localhost" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

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
    Just ["<iq id=\"id\" type=\"result\" xmlns=\"jabber:client\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test@localhost/" `append` (pack $ fromMaybe "" (Data.Text.unpack <$> r)) `append` "</jid></bind></iq>"]

-- Other Iq tests

-- info

test_infoHandler1 :: IO Bool
test_infoHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing to attribute
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (infoHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler2 :: IO Bool
test_infoHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (infoHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler3 :: IO Bool
test_infoHandler3 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- type is not get
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"result\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (infoHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler4 :: IO Bool
test_infoHandler4 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- wrong query namespace
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (infoHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler5 :: IO Bool
test_infoHandler5 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (infoHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"cat\" name=\"name\" type=\"type\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/></query></iq>"]

-- items

test_itemsHandler1 :: IO Bool
test_itemsHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing to attribute
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (itemsHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler2 :: IO Bool
test_itemsHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (itemsHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler3 :: IO Bool
test_itemsHandler3 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- type is not get
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"result\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (itemsHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler4 :: IO Bool
test_itemsHandler4 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (itemsHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler5 :: IO Bool
test_itemsHandler5 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (itemsHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>"]

-- ping

test_pingHandler1 :: IO Bool
test_pingHandler1 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing to attribute
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq (pingHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_pingHandler2 :: IO Bool
test_pingHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- missing ping
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (pingHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_pingHandler3 :: IO Bool
test_pingHandler3 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  -- type is not get
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"result\" to=\"t\" from=\"f\"></iq>" .| parseBytes def .| (receiveIq (pingHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_pingHandler4 :: IO Bool
test_pingHandler4 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"wrong\"/></iq>" .| parseBytes def .| (receiveIq (pingHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_pingHandler5 :: IO Bool
test_pingHandler5 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq (pingHandler sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"/>"]

-- errors

test_iqError :: IO Bool
test_iqError = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (iqError sink))
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
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"cat\" name=\"name\" type=\"type\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/></query></iq>"]

test_iqHandler2 :: IO Bool
test_iqHandler2 = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  print r
  print (fmap renderElement <$> sent)

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

  print r
  print (fmap renderElement <$> sent)

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
              , (test_infoHandler3, "infoHandler 3")
              , (test_infoHandler4, "infoHandler 4")
              , (test_infoHandler5, "infoHandler 5")
              , (test_itemsHandler1, "itemsHandler 1")
              , (test_itemsHandler2, "itemsHandler 2")
              , (test_itemsHandler3, "itemsHandler 3")
              , (test_itemsHandler4, "itemsHandler 4")
              , (test_itemsHandler5, "itemsHandler 5")
              , (test_pingHandler1, "pingHandler 1")
              , (test_pingHandler2, "pingHandler 2")
              , (test_pingHandler3, "pingHandler 3")
              , (test_pingHandler4, "pingHandler 4")
              , (test_pingHandler5, "pingHandler 5")
              , (test_iqError, "iqError")
              , (test_iqHandler1, "iqHandler 1")
              , (test_iqHandler2, "iqHandler 2")
              , (test_iqHandler3, "iqHandler 3")
              , (test_iqHandler4, "iqHandler 4") ]

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

testmsg2 :: ByteString
testmsg2 = "<message xmlns=\"jabber:client\" from=\"test\" id=\"0f41469e-55fe-42c8-88a2-997e592ef16d\" to=\"foo@localhost\" type=\"chat\">ahhhhh</message>"

testmsg :: ByteString
testmsg = "<message xmlns=\"jabber:client\" from=\"test@localhost/gajim.CD9NEZ09\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" to=\"foo@localhost\" type=\"chat\"><body>eoaueoa</body><origin-id xmlns=\"urn:xmpp:sid:0\" id=\"615a1f64-0d8a-44c1-8bfd-52b2fa5622dd\" /><request xmlns=\"urn:xmpp:receipts\" /><thread>MrwqjWfrzhgjYOPHfuQwOjgWuSTHWIcM</thread></message>"

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
