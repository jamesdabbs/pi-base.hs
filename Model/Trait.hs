module Model.Trait
( traitConsequences
, TraitCreateData (..)
, traitCreate
, TraitUpdateData (..)
, traitUpdate
, traitDelete
, traitStruts
, traitSupport
, traitValueBool
, traitPrefetch
) where

import Import hiding ((==.))
import Data.List (nub)
import Database.Esqueleto hiding (delete)
import qualified Database.Persist.Sql as SQL

import DB (supportedTraits, deleteWithConsequences, Prefetch, prefetch)
import Handler.Helpers (sendErrorMessage, invalid422)
import Model.Revision


data TraitCreateData = TraitCreateData
  { tcdSpace :: SpaceId
  , tcdProperty :: PropertyId
  , tcdValue :: TValueId
  , tcdDescription :: Textarea
  }

data TraitUpdateData = TraitUpdateData
  { tudDescription :: Textarea
  }

traitCreate :: TraitCreateData -> Handler (Entity Trait)
traitCreate d = do
  now <- lift getCurrentTime
  existing <- runDB . getBy $ TraitSP (tcdSpace d) (tcdProperty d)
  case existing of
    Just _ -> sendErrorMessage invalid422 "Space / property pair already exists"
    _ -> do
      let trait = Trait (tcdSpace d) (tcdProperty d) (tcdValue d) (tcdDescription d) now now False
      createWithRevision trait

traitUpdate :: Entity Trait -> TraitUpdateData -> Handler (Entity Trait)
traitUpdate (Entity _id t) d = do
  now <- lift getCurrentTime
  let updated = t { traitDescription = tudDescription d, traitUpdatedAt = now }
  updateWithRevision _id updated

traitConsequences :: TraitId -> Handler [Entity Trait]
traitConsequences _id = supportedTraits [_id]

traitDelete :: TraitId -> Handler Int64
traitDelete _id = do
  trait <- runDB $ get404 _id
  n <- deleteWithConsequences traitConsequences _id
  deleteWithRevision _id trait
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
traitValueBool t = case traitValueId t of
  TValueKey (SQL.SqlBackendKey 1) -> True
  TValueKey (SQL.SqlBackendKey 2) -> False
  _ -> error "Can't coerce traitValueId to Bool"

traitPrefetch :: [Entity Trait] -> Handler (Prefetch Space, Prefetch Property)
traitPrefetch ts = do
  spaces <- prefetch [SpaceId <-. pluck traitSpaceId ts]
  properties <- prefetch [PropertyId <-. pluck traitPropertyId ts]
  return (spaces, properties)
  where
    pluck f = nub . map (f . entityVal)
