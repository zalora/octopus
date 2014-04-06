module Network.Octopus.ThrottledIO where

import Control.Monad.IO.Class

class MonadIO m => ThrottledIO m where

