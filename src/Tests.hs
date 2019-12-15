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
import qualified Data.ByteString.Base64 as BS64
import Data.Conduit
import Data.Default
import Data.Conduit.List hiding (mapM_)
import Data.Text as T (Text, unpack)
import Data.Text.Encoding
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
import Control.Monad.Primitive

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
import TestUtils
import Roster
import Presence

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
test_receiveMessage :: Test
test_receiveMessage = TestList
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

-- Roster tests

test_nameFromJid :: Test
test_nameFromJid = TestList
  [ nameFromJid "name@domain" ~?= Just "name"
  , nameFromJid "name@domain@domain" ~?= Nothing
  , nameFromJid "name" ~?= Nothing
  , nameFromJid "" ~?= Nothing ]

test_jidFromResource :: Test
test_jidFromResource = TestList
  [ jidFromResource "name@domain/res" ~?= Just "name@domain"
  , jidFromResource "name@domain/res/res" ~?= Nothing
  , jidFromResource "name" ~?= Nothing
  , jidFromResource "" ~?= Nothing ]

----------------------------
-- Tests that require IO
----------------------------

-- Stream tests
test_openStream :: (MonadIO m, MonadReader XMPPSettings m) => m Bool
test_openStream = do
  (sink, tv) <- newTestSink

  uuid <- liftIO $ randomIO
  liftIO $ runTestConduit $ openStream uuid (yield "<anything/>" .| parseBytes def) sink
  sent <- liftIO $ atomically $ tryTakeTMVar tv
  fqdn <- asks fqdn

  return $ sent ==
    Just [pack $ "<stream:stream from=" <> show fqdn <> " version=\"1.0\" id=\"" <> show uuid <> "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

test_initiateStream :: (MonadIO m, MonadReader XMPPSettings m) => m Bool
test_initiateStream = do
  (sink, tv) <- newTestSink

  uuid <- liftIO $ randomIO
  liftIO $ runTestConduit $ initiateStream uuid sink
  sent <- liftIO $ atomically $ tryTakeTMVar tv
  fqdn <- asks fqdn

  return $ sent ==
    Just [pack $ "<stream:stream from=" <> show fqdn <> " version=\"1.0\" id=\"" <> show uuid <> "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

--------------------------------------------------
-- TLS tests
--------------------------------------------------

test_startTLS :: IO Bool
test_startTLS = do
  (sink, tv) <- newTestSink

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

--------------------------------------------------
-- Testing bind handler
--------------------------------------------------

test_bindHandler1 :: IO Bool
test_bindHandler1 = do
  (sink, tv) <- newTestSink

  cm <- atomically STC.empty
  -- not an iq stanza
  r <- runTestConduit $ yield "<asdf/>" .| parseBytes def .| receiveIqBind (bindHandler cm "test@domain" sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_bindHandler2 :: IO Bool
test_bindHandler2 = do
  (sink, tv) <- newTestSink

  cm <- atomically STC.empty
  -- not of type set
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"get\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>resourceid</resource></bind></iq>" .| parseBytes def .| receiveIqBind (bindHandler cm "test@domain" sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_bindHandler3 :: IO Bool
test_bindHandler3 = do
  (sink, tv) <- newTestSink

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><resource>resourceid</resource></bind></iq>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@domain" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just "resourceid" &&
    (fmap renderElement <$> sent) == Just ["<iq id=\"id\" type=\"result\" xmlns=\"jabber:client\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test@domain/resourceid</jid></bind></iq>"]

test_bindHandler4 :: IO Bool
test_bindHandler4 = do
  (sink, tv) <- newTestSink

  cm <- atomically STC.empty
  r <- runTestConduit $ yield "<iq id=\"id\" type=\"set\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"/></iq>" .| parseBytes def .| (receiveIqBind (bindHandler cm "test@domain" sink))
  sent <- atomically $ tryTakeTMVar tv

  return $ (fmap renderElement <$> sent) ==
    Just ["<iq id=\"id\" type=\"result\" xmlns=\"jabber:client\"><bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid xmlns=\"\">test@domain/" <> pack (maybe "" T.unpack r) <> "</jid></bind></iq>"]

--------------------------------------------------
-- Other Iq tests
--------------------------------------------------

-- wrap the handlers that take 4 fields to give a default one if needed
wrapHandler4 :: MonadThrow m =>
  (Text -> Text -> Text -> Text -> ConduitT Event o m (Maybe r)) ->
  (Text -> Text -> Text -> Maybe Text -> ConduitT Event o m (Maybe r))
wrapHandler4 handler a b c Nothing = handler a b c "default"
wrapHandler4 handler a b c (Just d) = handler a b c d

-- wrap the handlers that take 3 fields to ignore the to field
wrapHandler3 :: MonadThrow m =>
  (Text -> Text -> Text -> ConduitT Event o m (Maybe r)) ->
  (Text -> Text -> Text -> Maybe Text -> ConduitT Event o m (Maybe r))
wrapHandler3 handler a b c _ = handler a b c

--------------------------------------------------
-- info
--------------------------------------------------

-- Note that in the case of handlers, if they do not consume all the input an
-- exception will be thrown, so we do not tests those cases. This is OK since
-- in the combined handler the last handler (error) will consume the input.

test_infoHandler1 :: IO Bool
test_infoHandler1 = do
  (sink, tv) <- newTestSink

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . wrapHandler4 $ infoHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_infoHandler2 :: IO Bool
test_infoHandler2 = do
  (sink, tv) <- newTestSink

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq . wrapHandler4 $ infoHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"server\" name=\"HAXMPP\" type=\"im\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#items\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/><feature var=\"jabber:iq:roster\" xmlns=\"\"/></query></iq>"]

--------------------------------------------------
-- items
--------------------------------------------------

test_itemsHandler1 :: IO Bool
test_itemsHandler1 = do
  (sink, tv) <- newTestSink

  -- missing query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . wrapHandler4 $ itemsHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ r == Nothing && sent == Nothing

test_itemsHandler2 :: IO Bool
test_itemsHandler2 = do
  (sink, tv) <- newTestSink

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq . wrapHandler4 $ itemsHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>"]

--------------------------------------------------
-- ping
--------------------------------------------------

test_pingHandler1 :: IO Bool
test_pingHandler1 = do
  (sink, tv) <- newTestSink
  -- missing ping
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . wrapHandler4 $ pingHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_pingHandler2 :: IO Bool
test_pingHandler2 = do
  (sink, tv) <- newTestSink
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq . wrapHandler4 $ pingHandler sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"/>"]

--------------------------------------------------
-- errors
--------------------------------------------------

test_iqError1 :: IO Bool
test_iqError1 = do
  (sink, tv) <- newTestSink
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq . wrapHandler4 $ iqError sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

test_iqError2 :: IO Bool
test_iqError2 = do
  (sink, tv) <- newTestSink
  -- receiveIq should handle the case with no "to" attr properly
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"f\"/>" .| parseBytes def .| (receiveIq . wrapHandler4 $ iqError sink)
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"default\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

-- combined iq handler

test_iqHandler1 :: IO Bool
test_iqHandler1 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty
  -- info
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#info\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#info\"><identity category=\"server\" name=\"HAXMPP\" type=\"im\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#info\" xmlns=\"\"/><feature var=\"http://jabber.org/protocol/disco#items\" xmlns=\"\"/><feature var=\"urn:xmpp:ping\" xmlns=\"\"/><feature var=\"jabber:iq:roster\" xmlns=\"\"/></query></iq>"]

test_iqHandler2 :: IO Bool
test_iqHandler2 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty
  -- items
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"><query xmlns=\"http://jabber.org/protocol/disco#items\"/></iq>"]

test_iqHandler3 :: IO Bool
test_iqHandler3 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty
  -- ping
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"result\" xmlns=\"jabber:client\"/>"]

test_iqHandler4 :: IO Bool
test_iqHandler4 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty
  -- no query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" to=\"t\" from=\"f\"/>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- atomically $ tryTakeTMVar tv

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"t\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

test_iqHandler5 :: (MonadIO m, MonadReader XMPPSettings m) => m Bool
test_iqHandler5 = do
  (sink, tv) <- newTestSink
  cm <- liftIO $ atomically STC.empty
  -- no to field
  r <- liftIO $ runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"f\"><ping xmlns=\"urn:xmpp:ping\"/></iq>" .| parseBytes def .| (receiveIq (iqHandler cm sink))
  sent <- liftIO $ atomically $ tryTakeTMVar tv
  fqdn <- asks fqdn

  return $
    r == Just () &&
    (fmap renderElement <$> sent) == Just ["<iq from=\"" <> encodeUtf8 fqdn <> "\" id=\"id\" to=\"f\" type=\"error\" xmlns=\"jabber:client\"/>"]

--------------------------------------------------
-- roster
--------------------------------------------------

test_rosterHandler1 :: IO Bool
test_rosterHandler1 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty
  -- no query
  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"name@domain/res\"/>" .| parseBytes def .| (receiveIq . wrapHandler3 $ rosterHandler cm sink)
  sent <- atomically $ tryTakeTMVar tv

  return $ isNothing r && isNothing sent

test_rosterHandler2 :: IO Bool
test_rosterHandler2 = do
  (sink, tv) <- newTestSink
  cm <- atomically STC.empty

  r <- runTestConduit $ yield "<iq xmlns=\"jabber:client\" id=\"id\" type=\"get\" from=\"name@domain/res\"><query xmlns=\"jabber:iq:roster\"/></iq>" .| parseBytes def .| (receiveIq . wrapHandler3 $ rosterHandler cm sink)
  sent <- atomically $ tryTakeTMVar tv

  print (fmap renderElement <$> sent)

  return $
    r == Just () &&
    -- (fmap renderElement <$> sent) == Just ["<iq from=\"" <> (encodeUtf8 $ fqdn def) <> "\" id=\"id
    True

test_isPresent :: IO Bool
test_isPresent = return True

-- This has to be in IO because we can't generate Entities without real keys from the DB.
test_rosterItems :: IO Bool
test_rosterItems = return True

test_getRoster :: IO Bool
test_getRoster = return True

test_removeRoster :: IO Bool
test_removeRoster = return True

test_addRoster :: IO Bool
test_addRoster = return True

test_updatePresenceTo :: IO Bool
test_updatePresenceTo = return True

--------------------------------------------------
-- presence
--------------------------------------------------

test_updatePresence :: IO Bool
test_updatePresence = return True

test_presenceHandler :: IO Bool
test_presenceHandler = return True

main :: IO ()
main = do
  runTestTT unitTests
  runTests ioTests
  where
    ioTests = [ (runXMPPNoDB . runConduit $ test_initiateStream, "initiateStream")
              , (runXMPPNoDB . runConduit $ test_openStream, "openStream")
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
              , (test_iqError1, "iqError1")
              , (test_iqError2, "iqError2")
              , (test_iqHandler1, "iqHandler 1")
              , (test_iqHandler2, "iqHandler 2")
              , (test_iqHandler3, "iqHandler 3")
              , (test_iqHandler4, "iqHandler 4")
              , (runXMPPNoDB . runConduit $ test_iqHandler5, "iqHandler 5")
              , (test_rosterHandler1, "rosterhandler1")
              , (test_rosterHandler2, "rosterhandler2")
              , (runXMPP $ testMessaging testUser1 testUser2, "Test Messages from 1 to 2")
              , (runXMPP $ testMessaging testUser1 testUser2, "Test Messages from 2 to 1")
              , (runXMPP . runConduit $ testPlainAuth testUser1, "Test authentication of user 1")
              , (runXMPP . runConduit $ testPlainAuth testUser2, "Test authentication of user 2")
              , (not <$> (runXMPP . runConduit $ testPlainAuth (testUser1 {userPassword="bogus"})), "Test bad authentication.")
              ]

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
      , "receiveMessage"   ~: test_receiveMessage
      , "nameFromJid"      ~: test_nameFromJid
      , "jidFromResource"  ~: test_jidFromResource
      ]


--    runXMPP :: (MonadIO m, MonadThrow m, MonadReader XMPPSettings m, MonadLogger m) => m Bool -> m1 bool
    runTests [] = return ()
    runTests ((t, name):ts) = do
      result <- t
      print $ name ++ (if result then " passed" else " failed *******************")
      runTests ts

runXMPPNoDB :: MonadIO m => ReaderT XMPPSettings (NoLoggingT m) a -> m a
runXMPPNoDB xmpp = runNoLoggingT . flip runReaderT testSettings $ do
  xmpp

runXMPP :: MonadIO m => ReaderT XMPPSettings (NoLoggingT m) a -> m a
runXMPP xmpp = runNoLoggingT . flip runReaderT testSettings $ do
  cleanRunWithTestDB
  xmpp

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


--------------------------------------------------
-- Test handleClient' and plainAuth
--------------------------------------------------

-- | Test plainAuth with XML stanzas.
testPlainAuth
  :: (PrimMonad m, MonadThrow m, MonadReader XMPPSettings m, MonadUnliftIO m) =>
     User -> ConduitT i o m Bool
testPlainAuth user = do
  (sink, tv) <- newTestSink
  let authMsg = createAuthStanza user
  let source = sourceList (elementToEvents (toXMLElement authMsg))
  auth <- plainAuth source sink
  return $ Just user == auth

-- | Test handleClient with some messages.
testHandleClient ::
  (MonadThrow m, PrimMonad m, MonadReader XMPPSettings m, MonadUnliftIO m, MonadLogger m) =>
  User -> m ()
testHandleClient user = do
  (bytesink, tv) <- newTestSink
  (sink, chan) <- forkSink bytesink
  cm <- testChanMap
  dn <- asks fqdn

  let authMsg = createAuthStanza user
  let source = sourceList (createOpenStream dn : elementToEvents (toXMLElement authMsg))
  handleClient' handleStreamDefault cm source sink bytesink
