module Main where

import qualified Web.Scotty as S
import Network.Octopus.HTTP

main = S.scotty 8000 scotty
