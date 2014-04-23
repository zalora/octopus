{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Network.Octopus.HTTP where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe (fromJust)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty

import Control.Concurrent.STM (atomically, STM)

import Network.Octopus.Command (Command, runCommand, runCommandS, ChunkSource, ChunkChan)
import Network.Octopus.Jobs (jobs, JobName)
import qualified Network.Octopus.SerializableIO as SIO

import Data.Conduit.TMChan (sourceTMChan, TMChan)

command :: JobName -> IO Command
command name = jobs >>= return . fromJust . M.lookup name

simpleRunner :: JobName -> IO T.Text
simpleRunner name = command name >>= fmap T.fromStrict . runCommand

sourceRunner :: JobName -> IO ChunkSource
sourceRunner name = command name >>= runCommandS

run :: Parsable t => (t -> IO a) -> (a -> ActionM ()) -> ActionM ()
run runner liftOut = do
    name <- param "name"
    _user <- header "from"
    output <- liftIO $ runner name
    liftOut output

chanSource :: ChunkChan -> ActionM ()
chanSource = (source =<<) . liftIO . return . sourceTMChan

chanText :: TMChan T.Text -> ActionM ()
chanText = (text =<<) . liftIO . fmap fromJust . SIO.awaitResult

dispatch :: forall result. (SIO.SerializableIO Command result) => SIO.CommandQueueMap Command -> (TMChan result -> ActionM ()) -> ActionM ()
dispatch cmdQ = run $ \name -> command name >>= SIO.dispatchAction SIO.noOwner cmdQ

scotty :: ScottyM ()
scotty = do
    middleware logStdoutDev

    cmdQ <- liftIO $ atomically $ (SIO.queueMap :: STM (SIO.CommandQueueMap Command))

    get "/" $ json =<< liftIO jobs

    post "/concurrent/:name" $ run sourceRunner source

    post "/enqueue/:name" $ dispatch cmdQ chanSource
    post "/qplain/:name" $ dispatch cmdQ chanText
    get "/attach/:name" $ run ((fmap fromJust . atomically . SIO.attach =<<) . command) chanSource
