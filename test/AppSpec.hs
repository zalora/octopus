{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AppSpec (main, spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import           App

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (app "test/fixtures/jobs.yaml") $ do
  describe "GET /" $ do
    it "lists jobs" $ do
      get "/" `shouldRespondWith` [json|
        {
          processes: {
            commandText: "ps -ef",
            commandHost: "localhost"
          },
          messages: {
            commandText: "tail -10 /var/log/system.log",
            commandHost: "localhost"
          }
        }
      |]
