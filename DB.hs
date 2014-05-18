module DB
( matches'
, supportedTraits
, traitConsequences
, theoremConsequences
, deleteTrait
, deleteTheorem
, traitMap
, TraitMap
, addTrait
, addSupports
, theoremImplication
, propertyTheorems
, proofTraits
, proofTheorem
, derivedTraits
, spaceManualTraits
) where

import Import hiding ((==.), (!=.), delete)
import qualified Import as I (delete)

import Data.Int (Int64)
import Database.Esqueleto
import Data.List (partition, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time (getCurrentTime)

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

type TraitMap p = M.Map p (TraitId, TValueId)

traitMap :: SpaceId -> S.Set PropertyId -> Handler (TraitMap PropertyId)
traitMap sid ps = runDB $ do
  ets <- select $
    from $ \t -> do
    where_ $ (t ^. TraitSpaceId ==. val sid &&. t ^. TraitPropertyId `in_` (valList . S.toList $ ps))
    return t
  return . M.fromList . map (\(Entity tid t) -> (traitPropertyId t, (tid, traitValueId t))) $ ets

addTrait :: SpaceId -> PropertyId -> TValueId -> Text -> Handler (Entity Trait)
addTrait s p v d = do
  now <- liftIO getCurrentTime
  create $ Trait
      { traitSpaceId         = s
      , traitPropertyId      = p
      , traitValueId         = v
      , traitDescription     = d
      , traitDeduced         = True
      , traitCreatedAt       = now
      , traitUpdatedAt       = now
      }
  where
    create trait = do
      _id <- runDB . insert $ trait
      return $ Entity _id trait

theoremImplication :: Theorem -> Implication PropertyId
theoremImplication t = (Key . PersistInt64) <$> Implication (theoremAntecedent t) (theoremConsequent t)

propertyTheorems :: PropertyId -> Handler [Entity Theorem]
propertyTheorems pid = runDB . select $
  from $ \(t `InnerJoin` pt) -> do
  on (t ^. TheoremId ==. pt ^. TheoremPropertyTheoremId)
  where_ (pt ^. TheoremPropertyPropertyId ==. val pid)
  return t

proofTraits :: ProofId -> Handler [Entity Trait]
proofTraits pid = runDB . select $
  from $ \(assumptions `InnerJoin` traits) -> do
  on (assumptions ^. AssumptionTraitId ==. traits ^. TraitId)
  where_ (assumptions ^. AssumptionProofId ==. val pid)
  return traits

proofTheorem :: Proof -> Handler (Entity Theorem)
proofTheorem proof = do
  let _id = proofTheoremId proof
  theorem <- runDB . get404 $ _id
  return $ Entity _id theorem

derivedTraits :: TraitId -> Handler [Entity Trait]
derivedTraits _id = runDB . select $
  from $ \(assumptions `InnerJoin` proofs `InnerJoin` traits) -> do
  on (proofs ^. ProofTraitId ==. traits ^. TraitId)
  on (assumptions ^. AssumptionProofId ==. proofs ^. ProofId)
  where_ (assumptions ^. AssumptionTraitId ==. val _id)
  return traits

-- Supports are manually added traits used as assumptions, plus the supports of
--   any automatically added traits used as assumptions
addSupports :: TraitId -> S.Set TraitId -> Handler ()
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

spaceManualTraits :: SpaceId -> Handler [TraitId]
spaceManualTraits _id = do
  traits <- runDB . select $
    from $ \(traits) -> do
    where_ (traits ^. TraitSpaceId ==. (val _id) &&. traits ^. TraitDeduced ==. (val False))
    return traits
  return . map entityKey $ traits
