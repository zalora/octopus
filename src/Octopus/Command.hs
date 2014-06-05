{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Octopus.Command (
  Command(..)
, Host
, runCommand
, ChunkChan
, ChunkSource
, runCommandChan
, runCommandS
) where

import Prelude hiding (sequence, mapM)
import Data.String

import GHC.Generics (Generic)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS

import Control.Applicative
import Control.Monad (ap, join, mzero)
import Control.Monad.Trans

import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import GHC.IO.Handle (hClose, Handle)
import System.IO (hSetBuffering, BufferMode(..))
import System.Posix.IO (createPipe, fdToHandle)

import Data.Yaml
import Data.Conduit
-- import Data.Conduit.Combinators
import Blaze.ByteString.Builder

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMChan (TMChan, closeTMChan, writeTMChan)

data Host = Host T.Text
          deriving (Show, Ord, Eq, Generic)

instance IsString Host where
    fromString = Host . fromString

instance FromJSON Host where
    parseJSON v@(String _) = Host <$> parseJSON v
    parseJSON _ = mzero

instance ToJSON Host

data Command = Command { commandHost :: Host
                       , commandText :: T.Text
                       } deriving (Show, Ord, Eq, Generic)

instance FromJSON Command where
    parseJSON (Object v) = Command <$>
                           v .:? "host" .!= "localhost" <*>
                           v .: "command"
    parseJSON _ = mzero

instance ToJSON Command

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

runCreateProcess :: CreateProcess -> IO Handle
runCreateProcess proc = do
    (rh, wh) <- createPipeHandle
    hSetBuffering rh LineBuffering
    hSetBuffering wh LineBuffering
    (Just stdin, _, _, _) <- createProcess proc{std_out = UseHandle wh, std_err = UseHandle wh}
    hClose stdin
    return rh

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

commandProcess :: Command -> CreateProcess
commandProcess (Command (Host "localhost") args) = cmd "sh" ["-c", args]
commandProcess (Command (Host hostname) args) = cmd "ssh" ["-o", "StrictHostKeyChecking no", hostname, " ", args]

runCommand :: Command -> IO T.Text
runCommand = outputCreateProcess . commandProcess

runCommandS :: Command -> IO ChunkSource
runCommandS = sourceProc . commandProcess

runCommandChan :: ChunkChan -> Command -> IO ()
runCommandChan chan = 
    let consume handle = do
                          x <- BS.hGetSome handle 64
                          if BS.null x
                              then atomically $ closeTMChan chan
                              else do
                                atomically $ do
                                  writeTMChan chan $ Chunk $ fromByteString x
                                  writeTMChan chan Flush
                                consume handle
    in (consume =<<) . runCreateProcess . commandProcess
