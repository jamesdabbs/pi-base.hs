module DB
( matches'
, supportedTraits
, traitConsequences
, theoremConsequences
, deleteTrait
, deleteTheorem
) where

import Import hiding ((==.), (!=.), delete)
import qualified Import as I (delete)

import Data.Int (Int64)
import Database.Esqueleto

import Logic.Types

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

traitConsequences :: TraitId -> Handler [Entity Trait]
traitConsequences _id = supportedTraits [_id]

theoremConsequences :: TheoremId -> Handler [Entity Trait]
theoremConsequences _id = do
  proved <- runDB . select $
    from $ \(traits `InnerJoin` proofs) -> do
    on (traits ^. TraitId ==. proofs ^. ProofTraitId)
    where_ (proofs ^. ProofTheoremId ==. (val _id))
    return traits
  supportedTraits $ map entityKey proved

deleteConsequences :: [Entity Trait] -> Handler Int64
deleteConsequences _ids = runDB . deleteCount $ from $ \t ->
  where_ (t ^. TraitId `in_` (valList . map entityKey $ _ids))

deleteTrait :: TraitId -> Handler Int64
deleteTrait _id = do
  consequences <- traitConsequences _id
  runDB $ I.delete _id
  deleteConsequences consequences

deleteTheorem :: TheoremId -> Handler Int64
deleteTheorem _id = do
  consequences <- theoremConsequences _id
  runDB $ I.delete _id
  deleteConsequences consequences
