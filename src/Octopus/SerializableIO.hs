{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables  #-}
module Octopus.SerializableIO where

import Prelude hiding (sequence)
import Data.Traversable (sequence)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, newTQueue, readTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, newTVar, readTVar, writeTVar)

import System.IO.Unsafe (unsafePerformIO)

import Data.Conduit
import Data.Conduit.TMChan (dupTMChan, newTMChanIO, TMChan, writeTMChan, closeTMChan, readTMChan)
import Octopus.Command
import Blaze.ByteString.Builder

import Data.Time.Clock.POSIX (getPOSIXTime)

chanIO :: IO a -> TMChan a -> IO ()
chanIO action chan = do
    result <- action
    atomically $ do
      writeTMChan chan result
      closeTMChan chan

class (Ord ident) => SerializableIO ident a where
    runAction :: ident -> TMChan a -> IO ()

instance SerializableIO Command T.Text where
    runAction = chanIO . runCommand

instance SerializableIO Command LT.Text where
    runAction = chanIO . fmap LT.fromStrict . runCommand

sourcePool :: TVar (M.Map Command ChunkChan)
sourcePool = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE sourcePool #-}

attach :: Command -> STM (Maybe ChunkChan)
attach command = do
      m <- readTVar sourcePool
      sequence $ fmap dupTMChan $ M.lookup command m

instance SerializableIO Command (Flush Builder) where
    runAction command chan = do
      atomically $ do
        m <- readTVar sourcePool
        writeTVar sourcePool $ M.insert command chan m
      runCommandChan chan command

type TActionQueue = TQueue (IO ())
type CommandQueueMap ident = TVar (M.Map ident TActionQueue)
data Owner = Owner { lastRun :: Int }

queueMap :: (Ord a) => STM (CommandQueueMap a)
queueMap = newTVar M.empty

-- | Client interface
dispatchAction :: forall ident result. (SerializableIO ident result) => Owner -> CommandQueueMap ident -> ident -> IO (TMChan result)
dispatchAction _owner cmdQ ident = do
    qAction <- atomically $ actionQueue cmdQ ident
    queue <- qAction
    chan <- newTMChanIO :: IO (TMChan result)
    atomically $ do
      writeTQueue queue $ runAction ident chan
      writeTQueue queue teardown
    return chan

awaitResult :: TMChan a -> IO (Maybe a)
awaitResult chan = atomically $ do
  val <- readTMChan chan
  case val of
    Just v -> do
      closeTMChan chan
      return $ Just v
    Nothing -> return Nothing

teardown :: IO ()
teardown = threadDelay 5000000

-- | Returns a worker queue in an action that starts a queue if necessary,
--   adds in to a CommandQueueMap
actionQueue :: Ord ident => CommandQueueMap ident -> ident -> STM (IO TActionQueue)
actionQueue cmdQ ident = do
    qmap <- readTVar cmdQ
    case M.lookup ident qmap of
      Nothing -> do
        q <- newTQueue
        writeTVar cmdQ (M.insert ident q qmap)
        return $ worker q >> return q
      Just q -> return $ return q

worker :: TActionQueue -> IO ()
worker queue = forkIO loop >> return ()
  where
    loop = do
      action <- atomically $ readTQueue queue
      t <- getPOSIXTime
      putStrLn $ "running action " ++ (show (round t))
      action
      t' <- getPOSIXTime
      putStrLn $ "done " ++ (show (round t'))
      loop

noOwner :: Owner
noOwner = undefined -- TODO