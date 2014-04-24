{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Octopus.HTTP where

import Control.Monad.IO.Class (liftIO)

import qualified Data.Text.Lazy as T
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (mconcat)

import Network.Wai.Middleware.RequestLogger
import Web.Scotty
import Network.HTTP.Types.Status (unauthorized401)

import Control.Concurrent.STM (atomically, STM)

import Octopus.Command (Command, runCommandS, ChunkSource, ChunkChan)
import Octopus.Jobs (jobs, JobName)
import qualified Octopus.SerializableIO as SIO
import Octopus.Owner (canEnqueue, bumpOwner, OwnerMap, emptyOwnerMap, lookupCreateOwner)

import Data.Conduit.TMChan (sourceTMChan, TMChan)

command :: JobName -> IO Command
command name = jobs >>= return . fromJust . M.lookup name

sourceRunner :: JobName -> IO ChunkSource
sourceRunner name = command name >>= runCommandS

attachRunner :: JobName -> IO ChunkChan
attachRunner name = command name >>= (fmap fromJust . atomically . SIO.attach)

runAccounted :: Parsable t => (t -> IO a) -> OwnerMap -> (a -> ActionM ()) -> ActionM ()
runAccounted runner ownerMap liftOut = do
    name <- param "name"
    email <- header "from"
    owner <- liftIO $ atomically $ lookupCreateOwner email ownerMap
    eligible <- liftIO $ canEnqueue owner
    case eligible of
      True -> do
        owner' <- liftIO $ bumpOwner owner ownerMap -- we may want to bump *after* the action is finished
        addHeader "X-Owner" $ T.pack $ show owner'
        output <- liftIO $ runner name
        liftOut output
      False -> do
        status unauthorized401
        addHeader "X-Owner" $ T.pack $ show owner
        text $ mconcat ["quota exceeded: ", T.pack $ show owner, "\n"]

run :: Parsable t => (t -> IO a) -> (a -> ActionM ()) -> ActionM ()
run runner liftOut = do
    name <- param "name"
    output <- liftIO $ runner name
    liftOut output

chanSource :: ChunkChan -> ActionM ()
chanSource = (source =<<) . liftIO . return . sourceTMChan

chanText :: TMChan T.Text -> ActionM ()
chanText = (text =<<) . liftIO . fmap fromJust . SIO.awaitResult

dispatch :: forall result. (SIO.SerializableIO Command result) => SIO.CommandQueueMap Command -> OwnerMap -> (TMChan result -> ActionM ()) -> ActionM ()
dispatch cmdQ = runAccounted $ \name -> command name >>= SIO.dispatchAction cmdQ

scotty :: ScottyM ()
scotty = do
    middleware logStdoutDev

    cmdQ <- liftIO $ atomically $ (SIO.queueMap :: STM (SIO.CommandQueueMap Command))
    ownerMap <- liftIO $ atomically $ emptyOwnerMap

    get "/" $ json =<< liftIO jobs

    -- this call should be available only for admin users
    post "/concurrent/:name" $ run sourceRunner source

    post "/enqueue/:name" $ dispatch cmdQ ownerMap chanSource
    post "/qplain/:name" $ dispatch cmdQ ownerMap chanText
    get "/attach/:name" $ run attachRunner chanSource
