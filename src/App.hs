{-# LANGUAGE LambdaCase, OverloadedStrings, FlexibleContexts #-}
module App (app) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad

import           Data.ByteString.Builder (Builder)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy as T
import qualified Data.Conduit.List as CL
import           Data.Conduit (Source, Flush(..), ($$))

import           Network.Wai (Application)
import           Network.Wai.Middleware.Cors
import           Web.Scotty
import           Network.HTTP.Types.Status (unauthorized401)

import           Control.Concurrent.STM (atomically, STM, readTVar)

import           Octopus.Command
import           Octopus.Jobs
import qualified Octopus.SerializableIO as SIO
import           Octopus.Owner
import           Octopus.TQueue

import           Data.Conduit.TMChan (sourceTMChan, TMChan)

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

source :: Source IO (Flush Builder) -> ActionM ()
source src = stream $ \send flush ->
    src $$ CL.mapM_ (\case Chunk b -> send b
                           Flush -> flush)

chanSource :: ChunkChan -> ActionM ()
chanSource = source <=< return . sourceTMChan

chanText :: TMChan T.Text -> ActionM ()
chanText = text <=< liftIO . fmap fromJust . SIO.awaitResult

app :: FilePath -> IO Application
app jobFile = do
  cmdQ <- atomically $ (SIO.queueMap :: STM (SIO.ActionQueueMap Command))
  ownerMap <- atomically $ emptyOwnerMap

  scottyApp $ do
    middleware simpleCors

    get "/" $ json =<< liftIO jobs

    -- this call should be available only for admin users
    post "/concurrent/:name" $ run sourceRunner source

    get "/queue/:name" $ run ((\comm -> atomically $ readTVar cmdQ >>= dumpTQueue . fromJust . M.lookup comm) <=< command) json

    post "/enqueue/:name" $ dispatch cmdQ ownerMap chanSource
    post "/qplain/:name" $ dispatch cmdQ ownerMap chanText
    get "/attach/:name" $ setHeader "Cache-Control" "no-cache" >> run attachRunner chanSource

  where
    jobs :: IO JobsSpec
    jobs = readJobs jobFile

    command :: JobName -> IO Command
    command name = jobs >>= return . fromJust . M.lookup name

    sourceRunner :: JobName -> IO ChunkSource
    sourceRunner = runCommandS <=< command

    attachRunner :: JobName -> IO ChunkChan
    attachRunner = fmap fromJust . atomically . SIO.attach <=< command

    dispatch :: SIO.SerializableIO Command result => SIO.ActionQueueMap Command -> OwnerMap -> (TMChan result -> ActionM ()) -> ActionM ()
    dispatch cmdQ = runAccounted $ command >=> SIO.dispatchAction cmdQ
