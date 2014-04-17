{-# LANGUAGE OverloadedStrings #-}
module Network.Octopus.HTTP where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Control.Concurrent.STM (atomically, retry, STM)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Network.Octopus.Command (Command, runCommand, runCommandS)
import Network.Octopus.Jobs (jobs)
import qualified Network.Octopus.ThrottledIO as TIO

import Data.Conduit
import Data.Aeson
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay)
import Blaze.ByteString.Builder

run' name = jobs >>= return . fromJust . M.lookup name >>= fmap T.fromStrict . runCommand
runS' name = jobs >>= return . fromJust . M.lookup name >>= runCommandS
command name = jobs >>= return . fromJust . M.lookup name

run = do
    name <- param "name"
    output <- liftIO $ run' name
    text output

runSource = do
    name <- param "name"
    output <- liftIO $ runS' name
    source output

runQ cmdQ = do
    name <- param "name"
    output <- liftIO $ command name >>= TIO.dispatchAction TIO.noOwner cmdQ
    text output

scotty :: ScottyM ()
scotty = do
    middleware logStdoutDev

    cmdQ <- liftIO $ atomically $ (TIO.queueMap :: STM (TIO.CommandQueueMap Command))

    get "/" $ text "hello"
    post "/run/:name" runSource
    post "/q/:name" $ runQ cmdQ
