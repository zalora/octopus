{-# LANGUAGE OverloadedStrings #-}
module Network.Octopus.Jobs where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Yaml
import Data.Maybe (fromJust)

import Network.Octopus.Command (Command(..))

type JobName = T.Text
type JobsSpec = M.Map JobName Command

jobsFile :: IO FilePath
jobsFile = return "octopus.yaml"

jobs :: IO JobsSpec
jobs = jobsFile >>= fmap fromJust . decodeFile
