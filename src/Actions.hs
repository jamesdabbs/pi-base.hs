{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Actions
  ( searchByText
  , searchByFormula
  , getUniverse
  , assertTrait
  , assertTheorem
  ) where

import Base

import Control.Monad.State (runState)
import Control.Concurrent.MVar (readMVar, modifyMVar)
import qualified Data.Set as S
import Database.Persist
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Postgresql (runSqlPool)

import Logic (search, provisional, assertTheorem', assertTrait')
import Models
import Universe (moveTheorem)
import Util (encodeText)

searchByText :: Text -> Action [Entity Space]
searchByText _ = return []

searchByFormula :: Formula PropertyId -> MatchMode -> Action [Entity Space]
searchByFormula f m = do
  u <- getUniverse
  let ids = search f m u
  runDB $ selectList [SpaceId <-. ids] []

getUniverse :: Action Universe
getUniverse = asks getUVar >>= liftIO . readMVar

assertTrait :: Trait -> Action (Entity Trait)
assertTrait t = commit (assertTrait' t) $ \u ps -> do
  _id <- insert t
  return (u, ps, Entity _id t)

assertTheorem :: Implication -> Action (Entity Theorem)
assertTheorem i@(Implication ant con desc) = commit (assertTheorem' i) $ \u ps -> do
  let t = Theorem (encodeText ant) (encodeText con) desc ""
  _id <- insert t
  let replace p@(Proof' tr th as) = if th == provisional then (Proof' tr provisional as) else p
      ps' = map replace ps
      u'  = moveTheorem provisional _id u
  return (u', ps', Entity _id t)

commit :: (State Universe Deductions)
       -> (Universe -> Deductions -> ReaderT SqlBackend IO (Universe, Deductions, Entity a))
       -> Action (Entity a)
commit modifications presave = do
  uvar <- asks getUVar
  pool <- asks getPool

  liftIO $ modifyMVar uvar $ \u -> do
    let (proofs, u') = runState modifications u

    -- FIXME: start transaction
    --        clean this waaaaay up
    flip runSqlPool pool $ do
      (u'', proofs', result) <- presave u' proofs
      forM_ proofs' $ \(Proof' trait thrm assumptions) -> do
        tid     <- insert trait
        proofId <- insert $ Proof tid thrm Nothing
        forM_ (S.toList assumptions) $ \prop -> do
          -- FIXME: n+1
          t <- getBy $ TraitSP (traitSpaceId trait) prop
          case t of
            Nothing -> error $ "Failed to find assumed trait: " ++ (show prop)
            Just (Entity _id _) -> insert $ Assumption proofId _id
          -- TODO: save struts and supporters as well
      return (u'', result)
