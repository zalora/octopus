module Network.Octopus.ThrottledIO where

import qualified Data.Map as M
import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Applicative

import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Network.Octopus.Command

type TCommandQueue = TQueue (IO ())
type CommandQueueMap = TVar (M.Map Command TCommandQueue)
data Owner = Owner { lastRun :: Int }

queueMap :: STM CommandQueueMap
queueMap = newTVar M.empty

queue :: STM TCommandQueue
queue = newTQueue

-- commandIO :: TChan a -> Command -> IO ()
commandIO chan command = do
    result <- runCommand command
    atomically $ writeTChan chan result
    return ()

type CommandResult = T.Text

chanIO :: TChan CommandResult -> IO CommandResult -> IO ()
chanIO chan action = do
    result <- action
    atomically $ writeTChan chan result
    return ()

teardownIO :: IO ()
teardownIO = threadDelay 5000000

enqueue :: CommandQueueMap -> Owner -> Command -> STM (IO (TChan CommandResult))
enqueue cmdQ owner command = 
    let writeCommand comm q = do
              chan <- newTChan
              writeTQueue q $ chanIO chan $ runCommand comm
              writeTQueue q teardownIO
              return chan

        go (Left queue) = writeCommand command queue >>= \chan -> return $ worker owner queue >> return chan
        go (Right queue) = writeCommand command queue >>= return.return
    in do
        qmap <- readTVar cmdQ
        q <- case M.lookup command qmap of
                    Nothing -> queue >>= return . Left
                    Just queue -> return $ Right queue
        case q of
          Left q' -> writeTVar cmdQ (M.insert command q' qmap) >> go q
          _ -> go q

worker :: Owner -> TCommandQueue -> IO ()
worker owner queue = forkIO loop >> return ()
  where
    loop = do
      action <- atomically $ readTQueue queue
      putStrLn "running action"
      action
      putStrLn "done"
      loop

noOwner = Owner (-1)
