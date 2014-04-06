{-# LANGUAGE DeriveDataTypeable #-}
module Network.Octopus.Command where

import Data.Typeable
import Data.Data

import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Applicative
-- import Control.Monad.State
import Control.Concurrent.STM.TQueue

import Data.Aeson (FromJSON)
import System.Process (createProcess, waitForProcess, CreateProcess(..), CmdSpec(..), StdStream(..))
import GHC.IO.Handle (hClose)

data Host = Host T.Text
          deriving (Show)
data Command = Command Host T.Text
             deriving (Show)

data HostQueue = HostQueue (M.Map Host (TQueue Command))
data OctopusState = OctopusState { hostQueue :: HostQueue }

runCreateProcess :: CreateProcess -> IO T.Text
runCreateProcess cp = do
    (Just stdin, Just stdout, Just stderr, p) <- createProcess cp
    out <- TIO.hGetContents stdout
    hClose stdin
    _ <- waitForProcess p
    hClose stdout
    hClose stderr
    return out

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

runCommand :: Command -> IO T.Text
runCommand (Command (Host hostname) args) = runCreateProcess $ cmd "ssh" ["-o", "StrictHostKeyChecking no", hostname, " ", args]
