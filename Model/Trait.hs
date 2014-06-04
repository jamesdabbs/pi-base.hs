module Model.Trait
( traitConsequences
, traitDelete
) where

import Import
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
