{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Logic
  ( assertTrait'
  , assertTheorem'
  , check
  , search
  , provisional
  ) where

import Control.Monad.State (gets)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import qualified Data.Set as S

import Base
import Formula (neg)
import Models (true, false)
import qualified Universe as U
import Util (flatMapM, unionN, toSqlKey)

provisional :: TheoremId
provisional = toSqlKey (-1)

search :: Formula PropertyId -> MatchMode -> Universe -> [SpaceId]
search f mode = map fst . filter matches . U.toPairs
  where
    matches (_,props) = mode == (fst $ check props f)

counterexamples :: Implication PropertyId -> Universe -> [SpaceId]
counterexamples (Implication ant con _) = search f Yes
  where
    f = And [ant, neg con]

assertTrait' :: Trait -> State Universe Deductions
assertTrait' Trait{..} = do
  found <- gets $ U.lookup traitSpaceId traitPropertyId
  case found of
    -- TODO: differentiate "already known" from "incorrect"
    Just  _ -> return []
    Nothing -> do
      U.insertTrait traitSpaceId traitPropertyId traitValueId
      checkImplications traitSpaceId traitPropertyId

assertTheorem' :: Implication PropertyId -> State Universe Deductions
assertTheorem' i@(Implication ant con _) = do
  cxs <- gets $ counterexamples i
  if null cxs
     then do
       U.insertTheorem provisional i
       -- TODO: can tighten this query up
       unknowns <- gets $ search (Or [ant, con]) Unknown
       let apply s = applyTheorem s (provisional, i)
       flatMapM apply unknowns
     else return []

checkImplications :: SpaceId -> PropertyId -> State Universe Deductions
checkImplications sid pid = do
  rt     <- gets $ U.relevantTheorems pid
  next   <- flatMapM (applyTheorem sid) rt
  result <- flatMapM (\(Proof' t _ _) -> checkImplications sid $ traitPropertyId t) $ next
  return $ next ++ result

applyTheorem :: SpaceId -> (TheoremId, Implication PropertyId) -> State Universe Deductions
applyTheorem sid (tid, (Implication ant con _)) = do
  props <- gets $ U.attributes sid
  case check props ant of
    (No, _)         -> return []
    (Yes, evidence) -> force con evidence
    (Unknown, _)    -> case check props $ neg con of
      (Yes, evidence) -> force (neg ant) evidence
      _ -> return []

  where
    force :: Formula PropertyId -> Set PropertyId -> State U.Universe [Proof']
    force (Atom pid v) evidence = do
      present <- gets $ U.contains sid pid
      if present
         then return []
         else do
           let tv = if v then true else false
           U.insertTrait sid pid tv
           let trait = Trait sid pid tv "" True
           return [Proof' trait tid evidence]

    force (And sf) evidence = flatMapM (flip force evidence) sf
    force (Or  sf) evidence = do
      props <- gets $ U.attributes sid
      let subs     = map (\f -> (f, check props f)) sf
          yeses    = [f | (f, (    Yes, _)) <- subs]
          unknowns = [f | (f, (Unknown, _)) <- subs]
          extra    = unionN [ev | (_, (No, ev)) <- subs]
      if length yeses == 0 && length unknowns == 1
         then force (head unknowns) (evidence `S.union` extra)
         else return []

filterMatch :: MatchMode -> [(MatchMode, a)] -> Maybe a
filterMatch t pairs = listToMaybe [ts | (m, ts) <- pairs, m == t]

tValToBool :: TValueId -> Bool
tValToBool tv
  | tv == true  = True
  | tv == false = False
  | otherwise   = error "Can't coerce TVal to Bool"

-- TODO: should be able to clean this up
check :: U.Properties -> Formula PropertyId -> (MatchMode, Set PropertyId)
check ts (Atom p e) = case M.lookup p ts of
  Nothing -> (Unknown, S.empty)
  Just tv -> if e == tValToBool tv
     then (Yes, S.singleton p)
     else (No,  S.singleton p)
check ts (And  sf) =
  let
    subs    = map (check ts) sf
    no      = filterMatch No subs
    unknown = filterMatch Unknown subs
  in
    case no of
      Just evidence -> (No, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (Yes, unionN . map snd $ subs)
check ts (Or sf) =
  let
    subs    = map (check ts) sf
    yes     = filterMatch Yes subs
    unknown = filterMatch Unknown subs
  in
    case yes of
      Just evidence -> (Yes, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (No, unionN . map snd $ subs)
