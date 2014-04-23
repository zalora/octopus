{-# LANGUAGE OverloadedStrings #-}
module Network.OctopusSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

import Data.Text ()

import Network.Octopus.Command
import Network.Octopus.Jobs
-- import Network.Octopus.SerializableIO
import Network.Octopus.HTTP

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "feature-checklist" $ do
    it "reads a job list file" $ do
      jobsFile >>= (`shouldSatisfy` not.null)

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
