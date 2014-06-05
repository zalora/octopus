{-# LANGUAGE OverloadedStrings #-}
module Helper where

import           Test.Hspec.Wai
import           Network.Wai.Test
import           Data.Aeson
import           Data.Aeson.QQ
import           Language.Haskell.TH.Quote

shouldRespondWithJSON :: WaiSession SResponse -> Value -> WaiExpectation
shouldRespondWithJSON r v = r `shouldRespondWith` ResponseMatcher (MatchBody $ encode v) 200

json :: QuasiQuoter
json = aesonQQ
