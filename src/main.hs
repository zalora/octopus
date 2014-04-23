module Main where

import qualified Web.Scotty as S
import Octopus.HTTP

main :: IO ()
main = S.scotty 8000 scotty
