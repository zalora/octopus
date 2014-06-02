{-# LANGUAGE OverloadedStrings #-}
module Octopus.HTTPSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai

import           Octopus.HTTP

main :: IO ()
main = hspec spec

spec :: Spec
spec = before app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200
