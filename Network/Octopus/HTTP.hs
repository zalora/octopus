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
import Blaze.ByteString.Builder

command name = jobs >>= return . fromJust . M.lookup name

simpleRunner name = command name >>= fmap T.fromStrict . runCommand
sourceRunner name = command name >>= runCommandS

run runner liftOut = do
    name <- param "name"
    output <- liftIO $ runner name
    liftOut output

runQueued cmdQ = run (\name -> command name >>= TIO.dispatchAction TIO.noOwner cmdQ)

scotty :: ScottyM ()
scotty = do
    middleware logStdoutDev

    cmdQ <- liftIO $ atomically $ (TIO.queueMap :: STM (TIO.CommandQueueMap Command))

    get "/" $ text "hello"

    post "/concurrent/:name" $ run sourceRunner source

    post "/enqueue/:name" $ runQueued cmdQ source
    post "/qplain/:name" $ runQueued cmdQ text

    get "/attach/:name" $ do
      name <- param "name"
      chunks <- liftIO $ command name >>= atomically . TIO.attach
      case chunks of
        Just c -> source c
        Nothing -> text "nothing :("
