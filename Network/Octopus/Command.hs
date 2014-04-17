{-# LANGUAGE OverloadedStrings #-}

module Network.Octopus.Command where

import Prelude hiding (sequence, mapM)

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

import Data.Maybe
import Control.Applicative
import Control.Monad (ap, join)
import Control.Monad.Trans

import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import GHC.IO.Handle (hClose, hDuplicateTo, Handle(..))
import System.IO (hSetBuffering, BufferMode(..))
import System.Posix.IO (createPipe, fdToHandle)

import Data.Conduit
-- import Data.Conduit.Combinators
import Blaze.ByteString.Builder
import Debug.Trace

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, STM)
import Control.Concurrent.STM.TMChan

data Host = Host T.Text
          deriving (Show, Ord, Eq)
data Command = Command Host T.Text
             deriving (Show, Ord, Eq)

type ChunkSource = Source IO (Flush Builder)
type ChunkChan = TMChan (Flush Builder)

mapMBoth :: Monad m => (t -> m a) -> (t, t) -> m (a, a)
mapMBoth f (a, b) = return (,) `ap` f a `ap` f b

createPipeHandle :: IO (Handle, Handle)
createPipeHandle = join $ fmap (mapMBoth fdToHandle) createPipe

outputCreateProcess :: CreateProcess -> IO T.Text
outputCreateProcess cp = do
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cp
    out <- TIO.hGetContents stdout
    hClose stdin
    _ <- waitForProcess p
    hClose stdout
    hClose stderr
    return out

sourceHandle :: Handle -> ChunkSource
sourceHandle h = do
    loop
  where
    loop = do
        x <- liftIO $ BS.hGetSome h 64
        if BS.null x
            then return ()
            else do
              yield $ Chunk $ fromByteString x
              yield Flush
              loop

runCreateProcess :: CreateProcess -> IO Handle
runCreateProcess proc = do
    (rh, wh) <- createPipeHandle
    hSetBuffering rh LineBuffering
    hSetBuffering wh LineBuffering
    (Just stdin, _, _, p) <- createProcess proc{std_out = UseHandle wh, std_err = UseHandle wh}
    hClose stdin
    return rh

sourceProc :: CreateProcess -> IO ChunkSource
sourceProc = fmap sourceHandle . runCreateProcess

cmd :: FilePath -> [T.Text] -> CreateProcess
cmd proc args = CreateProcess { cmdspec = RawCommand proc $ T.unpack <$> args
                              , cwd = Nothing
                              , env = Nothing
                              , std_in = CreatePipe
                              , std_out = CreatePipe
                              , std_err = CreatePipe
                              , close_fds = True
                              , create_group = True
                              }

sshCommand :: Command -> CreateProcess
sshCommand (Command (Host hostname) args) = cmd "ssh" ["-o", "StrictHostKeyChecking no", hostname, " ", args]

runCommand :: Command -> IO T.Text
runCommand = outputCreateProcess . sshCommand

runCommandS :: Command -> IO ChunkSource
runCommandS = sourceProc . sshCommand

runCommandChan :: Command -> IO ChunkChan
runCommandChan comm = 
    let consume chan handle = do
                              x <- BS.hGetSome handle 64
                              if BS.null x
                                  then atomically $ closeTMChan chan
                                  else do
                                    atomically $ do
                                      writeTMChan chan $ Chunk $ fromByteString x
                                      writeTMChan chan Flush
                                    consume chan handle
    in do
    chan <- newBroadcastTMChanIO
    forkIO $ runCreateProcess (sshCommand comm) >>= consume chan
    return chan
