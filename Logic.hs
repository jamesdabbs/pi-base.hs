module Logic
( matches
, negate
, converse
, negation
, contrapositive
, apply
, apply'
, addProof
, candidates
, relevantTheorems
, counterexamples
) where

import Import hiding (negate)
import Prelude (head)

import Control.Monad (join, liftM2)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing, listToMaybe)
import qualified Data.Set as S
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

import DB (matches', traitMap, TraitMap, addTrait, addSupports, theoremImplication, propertyTheorems)
import Logic.Types
import Util (intersectionN, unionN, encodeText)


-- TODO: don't hardcode this
boolToValueId :: Bool -> TValueId
boolToValueId v = if v
  then (Key . PersistInt64 $ 1)
  else (Key . PersistInt64 $ 2)


negate :: Formula p -> Formula p
negate (Atom p v) = Atom p $ not v
negate (And  fs ) = Or  $ map negate fs
negate (Or   fs ) = And $ map negate fs

converse :: Implication p -> Implication p
converse (Implication a c) = Implication c a

negation :: Implication p -> Implication p
negation (Implication a c) = Implication (negate a) (negate c)

contrapositive :: Implication p -> Implication p
contrapositive = negation . converse

-- Find (ids of) spaces matching a formula
matches :: MatchType -> Formula PropertyId -> Handler (S.Set SpaceId)

-- A conjunction is true if all its parts are true and unknown or false if any
--   parts are unknown, respectively
matches Yes (And sf) = intersectionN <$> mapM (matches Yes) sf
matches e (And sf) = unionN <$> mapM (matches e) sf

-- A disjunction is false if all parts or false, unknown or true if any are
matches No (Or sf) = intersectionN <$> mapM (matches No) sf
matches e (Or sf) = unionN <$> mapM (matches e) sf

-- We have to go to the database to find the spaces that match an atom
matches e (Atom p v) = S.fromList <$> matches' p (boolToValueId v) e


check :: SpaceId -> Formula PropertyId -> Handler (Maybe (S.Set TraitId))
check s f = do
  ts <- traitMap s (formulaProperties f)
  return $ check' ts f

check' :: (Ord p) => TraitMap p -> Formula p -> Maybe (S.Set TraitId)
check' ts (And  sf ) = foldl1 (liftM2 S.union) $ map (check' ts) sf
check' ts (Or   sf ) = join . listToMaybe . filter isJust . map (check' ts) $ sf
check' ts (Atom p e) = case M.lookup p ts of
  Nothing    -> Nothing
  Just (t,v) -> if v == (boolToValueId e)
    then Just . S.singleton $ t
    else Nothing

apply :: TheoremId -> Implication PropertyId -> SpaceId -> Handler [Entity Trait]
apply a i s = do
  $(logDebug) $ "Applying " <> Text.pack (show i) <> " to space " <> encodeText s
  ts <- traitMap s (implicationProperties i)
  let found = apply' a i ts
  mapM (addProof s) $ found

type Assumptions = (TheoremId, S.Set TraitId)
type ProofData p = (p, TValueId, Assumptions)

apply' :: (Ord p) => TheoremId -> Implication p -> TraitMap p -> [ProofData p]
apply' thrm (Implication ant cons) ts = do
  case check' ts ant of
    Just as -> force ts (thrm, as) cons
    Nothing -> case check' ts (negate cons) of
      Just as' -> force ts (thrm, as') (negate ant)
      Nothing  -> [] -- Neither direction applies

force :: (Ord p) => TraitMap p -> Assumptions -> Formula p -> [ProofData p]

-- Forcing a conjunction simply forces each subformula separately
force ts as (And sf) = concat . map (force ts as) $ sf

-- Forcing a disjunction can only work if there is only one unknown
-- This is a little verbose, but should be lazy in the right places
force ts (a,as) (Or sf) =
  let
    evals    = map (\f -> (f, check' ts f)) sf
    unknowns = take 2 . filter (isNothing . snd) $ evals
    knowns   = filter (isJust . snd) evals
  in if length unknowns == 1
    then
      let extra = unionN $ map (fromJust . snd) knowns
      in force ts (a,(S.union as extra)) (fst . head $ unknowns)
    else
      [] -- Too many unknowns to force (or no unknowns)

force ts as (Atom p v) = case M.lookup p ts of
  Just _  -> [] -- Forced value is already known
  Nothing -> [(p, (boolToValueId v), as)]

addProof :: SpaceId -> ProofData PropertyId -> Handler (Entity Trait)
addProof s (p,v,(thrm,ts)) = do
  trait@(Entity _id _) <- addTrait s p v ""
  now <- liftIO getCurrentTime
  pid <- runDB . insert $ Proof _id thrm 0 now now
  mapM_ (runDB . insert . Assumption pid) . S.toList $ ts
  addSupports _id ts
  $(logDebug) $ "Added trait " <> (encodeText _id)
  return trait

imatch :: MatchType -> MatchType -> Implication PropertyId -> Handler (S.Set SpaceId)
imatch at ct = \(Implication ant cons) -> do
  a <- matches at ant
  c <- matches ct cons
  return $ a `S.intersection` c

counterexamples, candidates:: Implication PropertyId -> Handler (S.Set SpaceId)
counterexamples = imatch Yes No
candidates = imatch Yes Unknown

relevantTheorems :: Trait -> Handler [(TheoremId, Implication PropertyId)]
relevantTheorems t = do
  ents <- propertyTheorems . traitPropertyId $ t
  return . map (\(Entity _id thrm) -> (_id, theoremImplication thrm)) $ ents
