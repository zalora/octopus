{-# LANGUAGE OverloadedStrings #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           App

main :: IO ()
main = hspec spec

spec :: Spec
spec = before (app "octopus.yaml") $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
