module Main where

import qualified Web.Scotty as S
import Octopus.HTTP

import qualified System.Remote.Monitoring as Mon

main :: IO ()
main = do
    _ <- Mon.forkServer "localhost" 8001
    S.scotty 8000 scotty
