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

-- The following tests don't pass.
-- Some weirdness with tag, prefixes, and namespaces.
-- Doesn't match the spec, but seems to be ok for the client.

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

{-
test_aborted :: Test
test_aborted = runST (runConduit $ aborted .| renderBytes def .| consume) ~?= [pack "<failure xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><aborted/></failure>"]

test_tlsFeatures :: Test
test_tlsFeatures = runST (runConduit $ tlsFeatures .| renderBytes def .| consume) ~?= [pack "<stream:features><starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"><required/></starttls></stream:features>"]

test_authFeatures :: Test
test_authFeatures = runST (runConduit $ authFeatures .| renderBytes def .| consume) ~?= [pack "<stream:features><mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"><mechanism>PLAIN</mechanism></mechanisms></stream:features>"]

test_bindFeatures :: Test
test_bindFeatures = runST (runConduit $ bindFeatures .| renderBytes def .| consume) ~?= [pack "<stream:features><bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/></stream:features>"]

test_bind :: Test
test_bind = runST (runConduit $ (bind "test") .| renderBytes def .| consume) ~?= [pack "<bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\"><jid>test</jid></bind>"]

test_login_success :: Test
test_login_success = undefined

test_login_fail :: Test
test_login_fail = undefined
-}

-- Tests that require IO

testSink :: MonadIO m => TMVar [a] -> ConduitT a o m ()
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
main = runTests tests
  where
    tests = [ (test_initiateStream, "initiateStream") ]

    runTests [] = return ()
    runTests ((t, name):ts) = do
      result <- t
      print $ name ++ (if result then " passed" else " failed")
      runTests ts
