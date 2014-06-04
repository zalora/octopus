{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified System.Remote.Monitoring as Mon

import App

main :: IO ()
main = bracket (Mon.forkServer "localhost" 8001) (killThread .  Mon.serverThreadId) $ \_ -> do
    app >>= run 8000 . logStdoutDev
