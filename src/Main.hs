{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Network

xmppPort :: Int
xmppPort = 5222

settings :: ServerSettings
settings = serverSettings xmppPort "*"

handleClient :: AppData -> IO ()
handleClient ad = putStrLn "connected"

main :: IO ()
main = runTCPServer settings handleClient
