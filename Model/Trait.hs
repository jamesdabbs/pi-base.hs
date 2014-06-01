module Model.Trait
( traitConsequences
, traitDelete
) where

import Import
import DB (supportedTraits, deleteWithConsequences)

traitConsequences :: TraitId -> Handler [Entity Trait]
traitConsequences _id = supportedTraits [_id]

traitDelete :: TraitId -> Handler Int64
traitDelete _id = do
  n <- deleteWithConsequences traitConsequences _id
  runDB $ delete _id
  return n
