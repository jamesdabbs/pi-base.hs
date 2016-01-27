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
getUniverse = asks getTU >>= liftIO . readTVarIO

searchUniverse :: Universe -> Formula PropertyId -> MatchMode -> [SpaceId]
searchUniverse u f m = map fst $ filter (\(_,pm) -> matches pm m f) (M.toList $ uspaces u)

matches :: M.Map PropertyId TValueId -> MatchMode -> Formula PropertyId -> Bool
matches pm Yes (And sf) = all (matches pm Yes) sf
matches pm   m (And sf) = any (matches pm   m) sf
matches pm Yes (Or  sf) = any (matches pm Yes) sf
matches pm   m (Or  sf) = all (matches pm   m) sf
matches pm m (Atom p v) =
  let found = M.lookup p pm
  in case (m,v) of
    (Yes, True)  -> found == Just true
    (Yes, False) -> found == Just false
    (No,  True)  -> found == Just false
    (No,  False) -> found == Just true
    _ -> found == Nothing
