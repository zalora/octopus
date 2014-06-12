{-# LANGUAGE OverloadedStrings #-}
module Helper where

import           Test.Hspec.Wai
import           Test.Hspec.Wai.Internal
import           Network.HTTP.Types
import           Network.Wai.Test
import           Data.Aeson
import           Data.Aeson.QQ
import           Language.Haskell.TH.Quote

shouldRespondWithJSON :: WaiSession SResponse -> Value -> WaiExpectation
shouldRespondWithJSON r v = r `shouldRespondWith` ResponseMatcher 200 [(hContentType, "application/json")] (Just $ encode v)

json :: QuasiQuoter
json = aesonQQ
