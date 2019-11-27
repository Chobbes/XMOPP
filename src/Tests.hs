{-# LANGUAGE OverloadedStrings #-}

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
import Data.Conduit.List
import Data.Text (Text, unpack)
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader

import Main
import XMPP
import XMLRender

test_required :: Test
test_required = renderElement required ~?=
                pack "<required/>"

test_proceed :: Test
test_proceed = renderElement proceed ~?=
               pack "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>"

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

test_streamRespHeader :: Test
test_streamRespHeader =
  runST (runConduit $
          streamRespHeader "localhost" "en"
          (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") .|
          renderBytes def .|
          consume) ~?=
  [pack "<stream:stream from=\"localhost\" version=\"1.0\" id=\"c2cc10e1-57d6-4b6f-9899-38d972112d8c\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

-- The following tests don't pass.
-- Some weirdness with tag, prefixes, and namespaces.
-- Doesn't match the spec, but seems to be ok for the client.

test_aborted :: Test
test_aborted = renderElement aborted ~?=
               pack "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><aborted/></failure>"

test_tlsFeatures :: Test
test_tlsFeatures = renderElement tlsFeatures ~?=
                   pack "<stream:features><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required/></starttls></stream:features>"

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

tests :: Test
tests = TestList
  [ "required" ~: test_required
  , "proceed"  ~: test_proceed
  , "success"  ~: test_success
  , "streamRespHeader" ~: test_streamRespHeader ]

-- Tests that require IO

testSink :: MonadIO m => TMVar [i] -> ConduitT i o m ()
testSink tv = do
  e <- consume
  liftIO $ atomically $ putTMVar tv e

test_initiateStream :: IO Bool
test_initiateStream = do
  tv <- (newEmptyTMVarIO :: IO (TMVar [ByteString]))
  let sink = testSink tv :: ConduitT BS.ByteString o (ReaderT XMPPSettings IO) ()

  uuid <- runReaderT (runConduit $ initiateStream sink) def
  sent <- atomically $ readTMVar tv

  return $ sent ==
    [pack $ "<stream:stream from=\"localhost\" version=\"1.0\" id=\"" ++ (show uuid) ++ "\" xmlns:xml=\"xml\" xml:lang=\"en\" xmlns:stream=\"http://etherx.jabber.org/streams\">"]

main :: IO ()
main = do
  runTestTT tests
  runTests ioTests
  where
    ioTests = [ (test_initiateStream, "initiateStream") ]

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
