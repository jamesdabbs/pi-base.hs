{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Actions
  ( searchByText
  , searchByFormula
  , getUniverse
  , commit
  ) where

import Base

import Control.Monad.State (execState)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar (modifyTVar, readTVarIO)
import qualified Data.Map as M
import Database.Persist

import Logic (check)
import Models

searchByText :: Text -> Action [Entity Space]
searchByText _ = return []

searchByFormula :: Formula PropertyId -> MatchMode -> Action [Entity Space]
searchByFormula f m = do
  u <- getUniverse
  let ids = searchUniverse u f m
  runDB $ selectList [SpaceId <-. ids] []

getUniverse :: Action Universe
getUniverse = asks getUVar >>= liftIO . readTVarIO

searchUniverse :: Universe -> Formula PropertyId -> MatchMode -> [SpaceId]
searchUniverse u f m = map fst $ filter (\(_,pm) -> matches pm m f) (M.toList $ uspaces u)

-- TODO: roll this vvv into that ^^^
matches :: Properties -> MatchMode -> Formula PropertyId -> Bool
matches props mode f = mode == result
  where
    (result, _) = check props f

commit :: State Universe [Proof'] -> Action [TraitId]
commit modifications = do
  uvar <- asks getUVar
  liftIO . atomically . modifyTVar uvar $ execState modifications
  -- TODO: persist proofs to DB
  --       what about Universe state if something fails here vvv ?
  return []
