{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Actions
  ( searchByText
  , searchByFormula
  , getUniverse
  ) where

import Base

import Control.Concurrent.STM.TVar (readTVarIO)
import qualified Data.Map as M
import Database.Persist

import Logic (check)
import Models

searchByText :: Text -> Action [Entity Space]
searchByText _ = return []

-- TODO: option for matches / doesn't match / unknown
searchByFormula :: Formula PropertyId -> MatchMode -> Action [Entity Space]
searchByFormula f m = do
  u <- getUniverse
  let ids = searchUniverse u f m
  runDB $ selectList [SpaceId <-. ids] []

getUniverse :: Action Universe
getUniverse = asks getUVar >>= liftIO . readTVarIO

searchUniverse :: Universe -> Formula PropertyId -> MatchMode -> [SpaceId]
searchUniverse u f m = map fst $ filter (\(_,pm) -> matches pm m f) (M.toList $ uspaces u)

matches :: Properties -> MatchMode -> Formula PropertyId -> Bool
matches props mode f = mode == result
  where
    (result, _) = check props f
