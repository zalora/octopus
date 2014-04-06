{-# LANGUAGE OverloadedStrings #-}
module Network.Octopus.HTTP where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Network.Octopus.Command (Command, runCommand)
import Network.Octopus.Jobs (jobs)

run' name = jobs >>= return . fromJust . M.lookup name >>= fmap T.fromStrict . runCommand

run = do
    name <- param "name"
    output <- liftIO $ run' name
    text output

scotty :: ScottyM ()
scotty = do
    middleware logStdoutDev
    get "/" $ text "hello"
    post "/run/:name" run
