{-# LANGUAGE OverloadedStrings #-}
module OctopusSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck.Instances ()

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
