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

import qualified Data.Map as M
import Data.Maybe (listToMaybe, catMaybes)
import qualified Data.Set as S
import qualified Data.Text as Text
import Data.Time (getCurrentTime)

import DB (matches', addSupports, addStruts)
import Models
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
matches :: MatchType -> Formula PropertyId -> Handler (Set SpaceId)

-- A conjunction is true if all its parts are true and unknown or false if any
--   parts are unknown, respectively
matches Yes (And sf) = intersectionN <$> mapM (matches Yes) sf
matches e (And sf) = unionN <$> mapM (matches e) sf

-- A disjunction is false if all parts or false, unknown or true if any are
matches No (Or sf) = intersectionN <$> mapM (matches No) sf
matches e (Or sf) = unionN <$> mapM (matches e) sf

-- We have to go to the database to find the spaces that match an atom
matches e (Atom p v) = S.fromList <$> matches' p (boolToValueId v) e

filterMatch :: MatchType -> [(MatchType, Set TraitId)] -> [Set TraitId]
filterMatch t pairs = [ts | (m, ts) <- pairs, m == t]

-- TODO: should be able to clean this up
check' :: (Ord p) => TraitMap p -> Formula p -> (MatchType, Set TraitId)
check' ts (And  sf) =
  let
    subs    = map (check' ts) sf
    no      = listToMaybe $ filterMatch No subs
    unknown = listToMaybe $ filterMatch Unknown subs
  in
    case no of
      Just evidence -> (No, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (Yes, unionN . map snd $ subs)
check' ts (Or sf) =
  let
    subs    = map (check' ts) sf
    yes     = listToMaybe $ filterMatch Yes subs
    unknown = listToMaybe $ filterMatch Unknown subs
  in
    case yes of
      Just evidence -> (Yes, evidence)
      Nothing -> case unknown of
        Just _ -> (Unknown, S.empty)
        Nothing -> (No, unionN . map snd $ subs)
check' ts (Atom p e) = case M.lookup p ts of
  Nothing    -> (Unknown, S.empty)
  Just (t,v) -> if v == (boolToValueId e)
    then (Yes, S.singleton t)
    else (No,  S.singleton t)

apply :: TheoremId -> Implication PropertyId -> SpaceId -> Handler [TraitId]
apply a i s = do
  $(logDebug) $ "Applying " <> Text.pack (show i) <> " to space " <> encodeText s
  ts <- spaceTraitMap s (implicationProperties i)
  let found = apply' a i ts
  mids <- mapM (addProof s) found
  return $ catMaybes mids

type Assumptions = (TheoremId, Set TraitId)
type ProofData p = (p, TValueId, Assumptions)

apply' :: (Ord p) => TheoremId -> Implication p -> TraitMap p -> [ProofData p]
apply' thrm (Implication ant cons) ts = do
  case check' ts ant of
    (Yes, evidence) -> force ts (thrm, evidence) cons
    (No, _) -> []
    (Unknown, _) -> case check' ts (negate cons) of
      (Yes, evidence') -> force ts (thrm, evidence') (negate ant)
      _ -> []

force :: (Ord p) => TraitMap p -> Assumptions -> Formula p -> [ProofData p]

-- Forcing a conjunction simply forces each subformula separately
force ts as (And sf) = concat . map (force ts as) $ sf

-- Forcing a disjunction can only work if there is only one unknown
--   and the rest are known not to hold
force ts (thrm,evidence) (Or sf) =
  let
    subs      = map (\f -> (f, check' ts f)) sf
    yes       = [f | (f, (Yes,     _)) <- subs]
    unknown   = [f | (f, (Unknown, _)) <- subs]
    evidence' = unionN $ [ev | (_, (No, ev)) <- subs]
  in
    if null yes && length unknown == 1
      then force ts (thrm, evidence `S.union` evidence') (head unknown)
      else []

force ts as (Atom p v) = case M.lookup p ts of
  Just _  -> [] -- Forced value is already known
  Nothing -> [(p, (boolToValueId v), as)]

-- TODO: there is a bit of a race condition here, if another request
--   sets the trait between the getBy check and the insert.
-- It might be better to catch the insert error and then raise a 500
--   if the DB state differs from the thing we tried to assert.
addProof :: SpaceId -> ProofData PropertyId -> Handler (Maybe TraitId)
addProof s (p,v,(thrm,ts)) = do
  mt <- runDB . getBy $ TraitSP s p
  case mt of
    Just (Entity _id t) -> do
      if traitValueId t == v
        then return Nothing
        else error $ "Conflicting assertions for (" <> (show s) <> "," <> (show p) <> ")"
    Nothing -> do
      now <- liftIO getCurrentTime
      _id <- runDB . insert $ Trait
          { traitSpaceId         = s
          , traitPropertyId      = p
          , traitValueId         = v
          , traitDescription     = Textarea ""
          , traitDeduced         = True
          , traitCreatedAt       = now
          , traitUpdatedAt       = now
          }
      pid <- runDB . insert $ Proof _id thrm 0 now now
      mapM_ (runDB . insert . Assumption pid) . S.toList $ ts
      addSupports _id ts
      addStruts _id thrm ts
      $(logDebug) $ "Added trait " <> (encodeText _id)
      return $ Just _id

imatch :: MatchType -> MatchType -> Implication PropertyId -> Handler (Set SpaceId)
imatch at ct = \(Implication ant cons) -> do
  a <- matches at ant
  c <- matches ct cons
  return $ a `S.intersection` c

counterexamples, candidates:: Implication PropertyId -> Handler (Set SpaceId)
counterexamples = imatch Yes No
candidates = imatch Yes Unknown

relevantTheorems :: Trait -> Handler [(TheoremId, Implication PropertyId)]
relevantTheorems t = do
  ents <- propertyTheorems . traitPropertyId $ t
  return . map (\(Entity _id thrm) -> (_id, theoremImplication thrm)) $ ents
