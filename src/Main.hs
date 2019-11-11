{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Network
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

xmppPort :: Int
xmppPort = 5222

settings :: ServerSettings
settings = serverSettings xmppPort "*"

handleClient :: AppData -> IO ()
handleClient ad =
  runConduit $ appSource ad .| awaitForever (lift . print)

main :: IO ()
main = runTCPServer settings handleClient
