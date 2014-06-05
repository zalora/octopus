{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module AppSpec (main, spec) where

import           Helper
import           Test.Hspec
import           Test.Hspec.Wai

import           App

main :: IO ()
main = hspec spec

spec :: Spec
spec = before (app "test/fixtures/jobs.yaml") $ do
  describe "GET /" $ do
    it "lists jobs" $ do
      get "/" `shouldRespondWithJSON` [json|
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
