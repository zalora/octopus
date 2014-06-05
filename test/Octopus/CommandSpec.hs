{-# LANGUAGE OverloadedStrings #-}
module Octopus.CommandSpec (main, spec) where

import           Test.Hspec

import           Octopus.Command

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "runCommand" $ do
    it "runs a command on localhost" $ do
      runCommand (Command "localhost" "uname") >>= (`shouldSatisfy` (`elem` ["Darwin\n", "Linux\n"]))
