{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module XMPP where

{-
  Some global XMPP things like settings, maps to channels, etc.
-}

import Data.Text
import Data.Default
import Control.Concurrent.STM.Map as STC

import Control.Monad.Reader.Class
import Control.Monad.Reader
import Control.Monad.Logger

import Data.Conduit
import Data.Conduit.TMChan
import Text.XML

import Database.Persist
import Database.Persist.Sqlite

--------------------------------------------------
-- Global XMPP settings
--------------------------------------------------

-- | Stream version?
version :: Text
version = "1.0"

data XMPPSettings =
  XMPPSettings { fqdn     :: Text
               , xmppPort :: Int
               , xmppDB   :: Text
               }

instance Default XMPPSettings where
  def = XMPPSettings "localhost" 5222 "xmpp.db"

type XMPPMonad    = ReaderT XMPPSettings (LoggingT IO)

type JID          = Text
type XMPPResource = Text

type ChanMap      = Map JID ([XMPPResource], Map XMPPResource (TMChan Element))
