{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified System.Remote.Monitoring as Mon

import Octopus.HTTP

main :: IO ()
main = do
    _ <- Mon.forkServer "localhost" 8001
    app >>= run 8000 . logStdoutDev
