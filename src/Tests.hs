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
import Control.Monad
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Logger
import qualified Data.Map as M
import Control.Concurrent.STM.TMChan
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

import Main
import XMPP
import XMLRender
import Users
import Stream
import TLS
import SASL
import Iq
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
  [pack "<stream:stream from=\"localhost\" version=\"1.0\" id=\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_features :: Test
test_features = TestList
  [ renderElement (features []) ~?=
    pack "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"/>"
  , renderElement (features [NodeElement required]) ~?=
    pack "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><required/></stream:features>" ]

test_required :: Test
test_required = renderElement required ~?=
                pack "<required/>"

-- TLS tests

test_proceed :: Test
test_proceed = renderElement proceed ~?=
               pack "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>"

-- Has a few extra namespaces but seems to work with clients
test_tlsFeatures :: Test
test_tlsFeatures = renderElement tlsFeatures ~?=
                   pack "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required xmlns=\"\"/></starttls></stream:features>"

-- SASL tests

-- TODO check PLAIN
test_awaitAuth :: Test
test_awaitAuth = TestList
  [ join (runConduit $ yield "<auth xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\" mechanism=\"PLAIN\">AGdyYWluAGFzZGY=</auth>" .| parseBytes def .| awaitAuth) ~?=
    Just ("grain", "asdf")
  , join (runConduit $ yield "<notauth/>" .| parseBytes def .| awaitAuth) ~?=
    Nothing ]

test_success :: Test
test_success = renderElement success ~?=
               pack "<success xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"

tlsin :: Monad m => ConduitT i BS.ByteString m ()
tlsin = yield $ pack $ Prelude.unlines
        [ "<stream:stream"
        , "from='juliet@im.example.com'"
        , "to='im.example.com'"
        , "version='1.0'"
        , "xml:lang='en'"
        , "xmlns='jabber:client'"
        , "xmlns:stream='http://etherx.jabber.org/streams'>"
        , "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>" ]

-- The following tests don't pass.
-- Some weirdness with tag, prefixes, and namespaces.
-- Doesn't match the spec, but seems to be ok for the client.

test_aborted :: Test
test_aborted = renderElement aborted ~?=
               pack "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><aborted/></failure>"

test_authFeatures :: Test
test_authFeatures = renderElement authFeatures ~?=
                    pack "<stream:features><mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><mechanism>PLAIN</mechanism></mechanisms></stream:features>"

test_bindFeatures :: Test
test_bindFeatures = renderElement bindFeatures ~?=
                    pack "<stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>"

test_bind :: Test
test_bind = renderElement (bind "test") ~?=
            pack "<bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid>test</jid></bind>"

test_login_success :: Test
test_login_success = undefined

test_login_fail :: Test
test_login_fail = undefined

unitTests :: Test
unitTests = TestList
  [ "awaitName" ~: test_awaitName
  , "streamRespHeader" ~: test_streamRespHeader
  , "features" ~: test_features
  , "required" ~: test_required
  , "proceed"  ~: test_proceed
  , "tlsFeatures" ~: test_tlsFeatures
  , "awaitAuth" ~: test_awaitAuth
  , "success"  ~: test_success ]

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
  sent <- atomically $ readTMVar tv

  return $ sent ==
    [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_initiateStream :: IO Bool
test_initiateStream = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [ByteString]))
  let sink = testSink tv -- :: ConduitT BS.ByteString o (ReaderT XMPPSettings IO) ()

  uuid <- randomIO
  runTestConduit $ initiateStream uuid sink
  sent <- atomically $ readTMVar tv

  return $ sent ==
    [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ show uuid ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

-- TLS tests

test_startTLS :: IO Bool
test_startTLS = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [Element]))
  let sink = testSink tv

  runTestConduit $ startTLS (yield "<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>" .| parseBytes def) sink
  sent <- atomically $ readTMVar tv

  return $ (renderElement <$> sent) ==
     [ "<stream:features xmlns:stream=\"http://etherx.jabber.org/streams\"><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required xmlns=\"\"/></starttls></stream:features>"
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

main :: IO ()
main = do
  runTestTT unitTests
  runTests ioTests
  where
    ioTests = [ (test_initiateStream, "initiateStream")
              , (test_openStream, "openStream")
              , (test_startTLS, "startTLS")
              , (test_authenticate1, "authenticate 1")
              , (test_authenticate2, "authenticate 2") ]

    runTests [] = return ()
    runTests ((t, name):ts) = do
      result <- t
      print $ name ++ (if result then " passed" else " failed")
      runTests ts

--------------------------------------------------
-- Misc test stuff
--------------------------------------------------

testiq :: BS.ByteString
testiq = "<iq id=\"5ba62e81-cbbd-45cc-a20a-5abca191b55f\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>gajim.CD9NEZ09</resource></bind></iq>"

testmsg2 :: BS.ByteString
testmsg2 = "<message xmlns=\"jabber:client\" from=\"test\" id=\"0f41469e-55fe-42c8-88a2-997e592ef16d\" to=\"foo@localhost\" type=\"chat\">ahhhhh</message>"

testmsg :: BS.ByteString
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
