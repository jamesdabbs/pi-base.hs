module Model.Trait
( traitConsequences
, traitDelete
, traitSupport
, traitValueBool
) where

import Import hiding ((==.))
import Database.Esqueleto hiding (delete)

import DB (supportedTraits, deleteWithConsequences)
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

traitSupport :: TraitId -> Handler [Entity Trait]
traitSupport _id = runDB . select $
  from $ \(traits `InnerJoin` supporters) -> do
    on (traits ^. TraitId ==. supporters ^. SupporterAssumedId)
    where_ (supporters ^. SupporterImpliedId ==. val _id)
    return traits

traitValueBool :: Trait -> Bool
traitValueBool t = case traitValueId t of
  Key (PersistInt64 1) -> True
  Key (PersistInt64 2) -> False
  _ -> error "Can't coerce traitValueId to Bool"
