{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, OverloadedStrings #-}
module Octopus.SerializableIO where

import Prelude hiding (sequence)
import Data.Traversable (sequence)
import GHC.Generics (Generic)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid (mconcat)

import System.IO.Unsafe (unsafePerformIO)

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TQueue (TQueue, writeTQueue, newTQueue, readTQueue)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, newTVar, readTVar, writeTVar)
import Data.Conduit
import Data.Conduit.TMChan (dupTMChan, newTMChanIO, TMChan, writeTMChan, closeTMChan, readTMChan)
import Blaze.ByteString.Builder (Builder)
import Data.Aeson

import Octopus.Command

class (ToJSON ident, Ord ident) => SerializableIO ident a where
    runAction :: ident -> TMChan a -> IO ()

chanIO :: IO a -> TMChan a -> IO ()
chanIO action chan = do
    result <- action
    atomically $ do
      writeTMChan chan result
      closeTMChan chan

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

data Action = Action Value (IO ()) deriving (Generic)

instance ToJSON Action where
    toJSON (Action v _) = v

instance Show Action where
    show (Action v _) = "action: <" ++ (show v) ++ ">"

type TActionQueue = TQueue Action
type ActionQueueMap ident = TVar (M.Map ident TActionQueue)

queueMap :: (Ord a) => STM (ActionQueueMap a)
queueMap = newTVar M.empty

-- | Client interface
dispatchAction :: SerializableIO ident result => ActionQueueMap ident -> ident -> IO (TMChan result)
dispatchAction cmdQ ident = do
    qAction <- atomically $ actionQueue cmdQ ident
    queue <- qAction
    chan <- newTMChanIO :: IO (TMChan result)
    atomically $ do
      writeTQueue queue $ Action (toJSON ident) $ runAction ident chan
      writeTQueue queue $ Action (String "teardown") teardown
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
--   adds in to a ActionQueueMap
actionQueue :: Ord ident => ActionQueueMap ident -> ident -> STM (IO TActionQueue)
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
      Action info action <- atomically $ readTQueue queue
      t <- getPOSIXTime
      LBS.putStrLn $ mconcat ["running ", encode info, " ", LBS.pack $ show (round t :: Integer)]
      action
      t' <- getPOSIXTime
      LBS.putStrLn $ mconcat [encode info, ":done ", LBS.pack $ show (round t' :: Integer)]
      loop
