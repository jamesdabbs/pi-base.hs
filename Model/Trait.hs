module Model.Trait
( traitConsequences
, traitDelete
, traitStruts
, traitSupport
, traitValueBool
, traitPrefetch
) where

import Import hiding ((==.), on)
import Data.List (nub)
import Database.Esqueleto hiding (delete)

import DB (supportedTraits, deleteWithConsequences, Prefetch, prefetch)
import Model.Revision

traitConsequences :: TraitId -> Handler [Entity Trait]
traitConsequences _id = supportedTraits [_id]

traitDelete :: TraitId -> Handler Int64
traitDelete _id = do
  trait <- runDB $ get404 _id
  logDeletion $ Entity _id trait
  n <- deleteWithConsequences traitConsequences _id
  runDB $ delete _id
  return n

traitStruts :: TraitId -> Handler [Entity Theorem]
traitStruts _id = runDB . select $
  from $ \(theorems `InnerJoin` struts) -> do
    on (theorems ^. TheoremId ==. struts ^. StrutTheoremId)
    where_ (struts ^. StrutTraitId ==. val _id)
    return theorems

traitSupport :: TraitId -> Handler [Entity Trait]
traitSupport _id = runDB . select $
  from $ \(traits `InnerJoin` supporters) -> do
    on (traits ^. TraitId ==. supporters ^. SupporterAssumedId)
    where_ (supporters ^. SupporterImpliedId ==. val _id)
    return traits

traitValueBool :: Trait -> Bool
traitValueBool t = case keyToValues $ traitValueId t of
  [PersistInt64 1] -> True
  [PersistInt64 2] -> False
  _ -> error "Can't coerce traitValueId to Bool"

traitPrefetch :: [Entity Trait] -> Handler (Prefetch Space, Prefetch Property)
traitPrefetch ts = do
  spaces <- prefetch [SpaceId <-. pluck traitSpaceId ts]
  properties <- prefetch [PropertyId <-. pluck traitPropertyId ts]
  return (spaces, properties)
  where
    pluck f = nub . map (f . entityVal)
