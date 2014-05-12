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
, theoremImplication
, propertyTheorems
) where

import Import hiding ((==.), (!=.), delete)
import qualified Import as I (delete)

import Data.Int (Int64)
import Database.Esqueleto
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

addTrait :: SpaceId -> PropertyId -> TValueId -> Text -> Handler (TraitId)
addTrait s p v d = do
  now <- liftIO getCurrentTime
  _id <- runDB . insert $ Trait
      { traitSpaceId         = s
      , traitPropertyId      = p
      , traitValueId         = v
      , traitDescription     = d
      , traitDeduced         = True
      , traitCreatedAt       = now
      , traitUpdatedAt       = now
      }
  _ <- error "Need to create node inline here"
  return _id

theoremImplication :: Theorem -> Implication PropertyId
theoremImplication t = (Key . PersistInt64) <$> Implication (theoremAntecedent t) (theoremConsequent t)

propertyTheorems :: PropertyId -> Handler [Entity Theorem]
propertyTheorems pid = runDB . select $
  from $ \(t `InnerJoin` pt) -> do
  on (t ^. TheoremId ==. pt ^. TheoremPropertyTheoremId)
  where_ (pt ^. TheoremPropertyPropertyId ==. val pid)
  return t
