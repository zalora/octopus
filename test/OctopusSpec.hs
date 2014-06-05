{-# LANGUAGE OverloadedStrings #-}
module OctopusSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

import Data.Text ()

import Octopus.Command

main :: IO ()
main = hspec spec

-- sorry i was using this like a todo-list

spec :: Spec
spec = do
  describe "feature-checklist" $ do
    it "spawns a http server" $ do
      True

    it "starts remote jobs from a job list file" $ do
      pending

    it "maintains persistent SSH connections" $ do
      pending

    it "throttles users" $ do
      pending


  describe "commands" $ do
    it "runs a command on localhost" $ do
      runCommand (Command (Host "localhost") "uname") >>= (`shouldSatisfy` (`elem` ["Darwin\n", "Linux\n"]))
