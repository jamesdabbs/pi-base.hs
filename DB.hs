{-# LANGUAGE ScopedTypeVariables #-}
module DB
( matches'
, addSupports
, derivedTraits
, supportedTraits
, deleteWithConsequences
, flushDeductions
, addStruts
, Prefetch
, prefetch
, icontains
) where

import Import hiding ((==.), (!=.), delete)

import Database.Esqueleto
import Data.List (partition, nub)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

type Prefetch a = M.Map (Key a) a

icontains :: EntityField r Text -> Text -> Filter r
icontains field v = Filter field (Left $ T.concat ["%", v, "%"]) (BackendSpecificFilter "ILIKE")

prefetch :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => [Filter e] -> Handler (Prefetch e)
prefetch f = do
  ents <- runDB $ selectList f []
  return . M.fromList . map (\e -> (entityKey e, entityVal e)) $ ents

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

deleteStruts :: [TraitId] -> Handler ()
deleteStruts ids = runDB . delete $
  from $ \s -> where_ (s ^. StrutTraitId `in_` (valList ids))

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
  deleteStruts ids
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

getStruts :: Set TraitId -> Handler (Set TheoremId)
getStruts ts = do
  ss <- runDB . select $
    from $ \(struts) -> do
    where_ (struts ^. StrutTraitId `in_` (valList . S.toList $ ts))
    return struts
  return . S.fromList . map (strutTheoremId . entityVal) $ ss

addStruts :: TraitId -> TheoremId -> Set TraitId -> Handler ()
addStruts trait theorem assumptions = do
  _ <- create theorem
  struts <- getStruts assumptions
  mapM_ create . S.toList $ struts
  where
    create theoremId = runDB . insert $ Strut theoremId trait

flushDeductions :: Handler ()
flushDeductions = do
  traits <- runDB . select $
    from $ \(t) -> do
    where_ (t ^. TraitDeduced ==. val True)
    return t
  _ <- deleteConsequences traits
  return ()
