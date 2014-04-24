{-# LANGUAGE RecordWildCards #-}

module Octopus.Owner (
  Owner
, canEnqueue

, OwnerMap
, emptyOwnerMap
, lookupCreateOwner
, bumpOwner
) where

import qualified Data.Text.Lazy as LT
import qualified Data.Map.Strict as M

import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)

type Email = LT.Text
data Owner = Owner { ownerId :: Maybe Email
                   , ownerLastRun :: POSIXTime
                   , ownerRunInterval :: NominalDiffTime
                   } deriving (Show, Eq)

owner :: Maybe Email -> Owner
owner o = Owner { ownerId = o
                , ownerLastRun = 0
                , ownerRunInterval = 600
                }

canEnqueue :: Owner -> IO Bool
canEnqueue Owner{..} = do
    now <- getPOSIXTime
    return $ now - ownerLastRun > ownerRunInterval
  
type OwnerMap = TVar (M.Map Email Owner)

emptyOwnerMap :: STM OwnerMap
emptyOwnerMap = newTVar M.empty

lookupOwner :: Email -> OwnerMap -> STM (Maybe Owner)
lookupOwner email tmap = readTVar tmap >>= return . M.lookup email

insertOwner :: Owner -> OwnerMap -> STM ()
insertOwner ownr@Owner{ownerId = Just ident} tmap = readTVar tmap >>= writeTVar tmap . M.insert ident ownr
insertOwner Owner{ownerId = Nothing} _ = return ()

lookupCreateOwner :: Maybe Email -> OwnerMap -> STM Owner
lookupCreateOwner Nothing _omap = return $ owner Nothing
lookupCreateOwner (Just email) tmap = lookupOwner email tmap >>= maybe (insertOwner newOwner tmap >> return newOwner) return
  where
    newOwner = owner $ Just email

bumpOwner :: Owner -> OwnerMap -> IO Owner
bumpOwner ownr tmap = ownerUpdateTime >>= \ownr' -> atomically $ (`insertOwner` tmap) ownr' >> return ownr'
  where
    ownerUpdateTime = getPOSIXTime >>= \time -> return ownr{ ownerLastRun = time }

