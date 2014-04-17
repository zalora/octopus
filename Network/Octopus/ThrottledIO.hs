{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Network.Octopus.ThrottledIO where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Control.Monad.IO.Class
import Control.Applicative

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TBMChan

import System.IO.Unsafe (unsafePerformIO)

import Data.Conduit
import Data.Conduit.TMChan (sourceTMChan, dupTMChan)
import Network.Octopus.Command

class (Ord ident) => SerializableIO ident a where
    runAction :: ident -> IO a
    
instance SerializableIO Command T.Text where
    runAction = runCommand

instance SerializableIO Command LT.Text where
    runAction = fmap LT.fromStrict . runCommand

sourcePool :: TVar (M.Map Command ChunkChan)
sourcePool = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE sourcePool #-}

attach :: Command -> STM (Maybe ChunkSource)
attach command = do
      m <- readTVar sourcePool
      case M.lookup command m of
        Just chan -> dupTMChan chan >>= return . Just . sourceTMChan
        _ -> return $ Nothing

instance SerializableIO Command ChunkSource where
    runAction command = do
      chan <- runCommandChan command
      readChan <- atomically $ do
        m <- readTVar sourcePool
        writeTVar sourcePool $ M.insert command chan m
        dupTMChan chan
      return $ sourceTMChan $ readChan

type TActionQueue = TQueue (IO ())
type CommandQueueMap ident = TVar (M.Map ident TActionQueue)
data Owner = Owner { lastRun :: Int }

queueMap :: (Ord a) => STM (CommandQueueMap a)
queueMap = newTVar M.empty

queue :: STM TActionQueue
queue = newTQueue

-- | Client interface
dispatchAction :: (SerializableIO ident result) => Owner -> CommandQueueMap ident -> ident -> IO result
dispatchAction owner cmdQ ident = do
    chanAction <- atomically $ enqueue cmdQ owner ident
    chan <- chanAction
    atomically $ readTChan chan

chanIO :: TChan a -> IO a -> IO ()
chanIO chan action = do
    result <- action
    atomically $ writeTChan chan result
    return ()

teardownIO :: IO ()
teardownIO = threadDelay 5000000

enqueue :: (SerializableIO ident result) => CommandQueueMap ident -> Owner -> ident -> STM (IO (TChan result))
enqueue cmdQ owner ident = 
    let writeCommand q = do
              chan <- newTChan
              writeTQueue q $ chanIO chan $ runAction ident
              writeTQueue q teardownIO
              return chan

        go (Left queue) = writeCommand queue >>= \chan -> return $ worker owner queue >> return chan
        go (Right queue) = writeCommand queue >>= return.return
    in do
        qmap <- readTVar cmdQ
        q <- case M.lookup ident qmap of
                    Nothing -> queue >>= return . Left
                    Just queue -> return $ Right queue
        case q of
          Left q' -> writeTVar cmdQ (M.insert ident q' qmap) >> go q
          _ -> go q

worker :: Owner -> TActionQueue -> IO ()
worker owner queue = forkIO loop >> return ()
  where
    loop = do
      action <- atomically $ readTQueue queue
      putStrLn "running action"
      action
      putStrLn "done"
      loop

noOwner = undefined -- TODO
