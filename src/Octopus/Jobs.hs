{-# LANGUAGE OverloadedStrings #-}
module Octopus.Jobs (JobsSpec, JobName, readJobs) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Yaml
import Data.Maybe (fromJust)

import Octopus.Command (Command(..))

type JobName = T.Text
type JobsSpec = M.Map JobName Command

readJobs :: FilePath -> IO JobsSpec
readJobs = fmap fromJust . decodeFile
