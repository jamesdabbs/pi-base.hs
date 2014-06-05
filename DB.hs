{-# LANGUAGE ScopedTypeVariables #-}
module DB
( matches'
, addSupports
, derivedTraits
, supportedTraits
, deleteWithConsequences
, flushDeductions
, pluck
) where

import Import hiding ((==.), (!=.), delete)

import Database.Esqueleto
import Data.List (partition, nub)
import qualified Data.Set as S

matches' :: PropertyId -> TValueId -> MatchType -> Handler [SpaceId]
matches' p v Yes = runDB . fmap (map entityKey) . select $
  from $ \(s `InnerJoin` t) -> do
  on (s ^. SpaceId ==. t ^. TraitSpaceId)
  where_ (t ^. TraitPropertyId ==. (val p) &&. t ^. TraitValueId ==. (val v))
  return s
matches' p v No = runDB . fmap (map entityKey) . select $
  from $ \(s `InnerJoin` t) -> do
  on (s ^. SpaceId ==. t ^. TraitSpaceId)
  where_ (t ^. TraitPropertyId ==. (val p) &&. t ^. TraitValueId !=. (val v))
  return s
matches' p _ Unknown = runDB $ do
  ts <- select $
    from $ \t -> do
    where_ (t ^. TraitPropertyId ==. (val p))
    return t
  let knownIds = map (traitSpaceId . entityVal) $ ts
  ss <- select $
    from $ \s -> do
    where_ (s ^. SpaceId `notIn` (valList knownIds))
    return s
  return . map entityKey $ ss

supportedTraits :: [TraitId] -> Handler [Entity Trait]
supportedTraits _ids = runDB . select $
  from $ \(traits `InnerJoin` supporters) -> do
    on (traits ^. TraitId ==. supporters ^. SupporterImpliedId)
    where_ (supporters ^. SupporterAssumedId `in_` (valList _ids))
    return traits

deleteSupports :: [TraitId] -> Handler ()
deleteSupports ids = runDB . delete $
  from $ \s -> where_ (s ^. SupporterImpliedId `in_` (valList ids))

deleteProofs :: [TraitId] -> Handler ()
deleteProofs ids = do
  proofs <- runDB . select $ from $ \p -> do
    where_ (p ^. ProofTraitId `in_` (valList ids))
    return p
  let pids = valList $ map entityKey proofs
  runDB . delete $ from $ \a -> where_ (a ^. AssumptionProofId `in_` pids)
  runDB . delete $ from $ \p -> where_ (p ^. ProofId `in_` pids)

deleteConsequences :: [Entity Trait] -> Handler Int64
deleteConsequences traits = do
  let ids = map entityKey traits
  deleteSupports ids
  deleteProofs ids
  runDB . deleteCount $ from $ \t -> where_ (t ^. TraitId `in_` (valList ids))

deleteWithConsequences :: (Key a -> Handler [Entity Trait]) -> Key a -> Handler Int64
deleteWithConsequences finder _id = do
  cons <- finder _id
  deleteConsequences cons

derivedTraits :: TraitId -> Handler [Entity Trait]
derivedTraits _id = runDB . select $
  from $ \(assumptions `InnerJoin` proofs `InnerJoin` traits) -> do
  on (proofs ^. ProofTraitId ==. traits ^. TraitId)
  on (assumptions ^. AssumptionProofId ==. proofs ^. ProofId)
  where_ (assumptions ^. AssumptionTraitId ==. val _id)
  return traits

-- Supports are manually added traits used as assumptions, plus the supports of
--   any automatically added traits used as assumptions
addSupports :: TraitId -> Set TraitId -> Handler ()
addSupports _id assumedIds = do
  traits <- runDB . select $
    from $ \(trait) -> do
    where_ (trait ^. TraitId `in_` (valList . S.toList $ assumedIds))
    return trait
  let (deduced, manual) = partition (traitDeduced . entityVal) traits
  supports <- runDB . selectDistinct $
    from $ \(supporters) -> do
    where_ (supporters ^. SupporterImpliedId `in_` (valList . ids $ deduced))
    return supporters
  let manualIds = ids manual
  let supportIds = map (supporterAssumedId . entityVal) supports
  runDB . mapM_ addSupport . nub $ manualIds ++ supportIds
  where
    ids = map entityKey
    addSupport aid = insert $ Supporter { supporterAssumedId = aid, supporterImpliedId = _id }

-- TODO: there's got to be a more concise way to express this ...
flushDeductions :: Handler ()
flushDeductions = do
  runDB . delete $
    from $ \(_ :: SqlExpr (Entity Supporter)) ->
    return ()
  runDB . delete $
    from $ \(_ :: SqlExpr (Entity Assumption)) ->
    return ()
  runDB . delete $
    from $ \(_ :: SqlExpr (Entity Proof)) ->
    return ()
  runDB . delete $
    from $ \(table) ->
    where_ (table ^. TraitDeduced ==. (val True))
  return ()

-- TODO: type signature
pluck f = do
  ents <- runDB $ selectList [] []
  return . map (f . entityVal) $ ents

