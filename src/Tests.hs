{-# LANGUAGE OverloadedStrings #-}
module Tests where

import Test.HUnit
import Control.Monad.ST
import Text.XML.Stream.Render
import Data.ByteString.Char8
import Data.Conduit
import Data.Default
import Data.Conduit.List
import Data.Text (Text, unpack)

import Main

test_required :: Test
test_required = runST (runConduit $ required .| renderBytes def .| consume) ~?= [pack "<required/>"]

test_proceed :: Test
test_proceed = runST (runConduit $ proceed .| renderBytes def .| consume) ~?= [pack "<proceed xmlns=\"urn:ietf:params:xml:ns:xmpp-tls\"/>"]

test_success :: Test
test_success = runST (runConduit $ success .| renderBytes def .| consume) ~?= [pack "<success xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"]

-- The following tests don't pass.
-- Some weirdness with tag, prefixes, and namespaces.
-- Doesn't match the spec, but seems to be ok for the client.

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
