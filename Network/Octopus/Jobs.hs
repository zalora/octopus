{-# LANGUAGE OverloadedStrings #-}
module Network.Octopus.Jobs where

import GHC.Generics

import Control.Applicative
import Control.Monad (mzero)

import System.FilePath
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Aeson (withText)
import Data.Yaml
import Data.Maybe (fromJust)

import Network.Octopus.Command (Command(..), Host(..))

type JobName = T.Text
type JobsSpec = M.Map JobName Command

instance FromJSON Host where
    parseJSON v@(String _) = Host <$> parseJSON v
    parseJSON _ = mzero

instance FromJSON Command where
    parseJSON (Object v) = Command <$>
                           v .: "host" <*>
                           v .: "command"

jobsFile :: IO FilePath
jobsFile = return "octopus.yaml"

jobs :: IO JobsSpec
jobs = jobsFile >>= fmap fromJust . decodeFile
