module Main where

import qualified Web.Scotty as S
import Network.Octopus.HTTP

main :: IO ()
main = S.scotty 8000 scotty
