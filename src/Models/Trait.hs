module Models.Trait
  ( create
  , update
  , delete
  ) where

import Base
import qualified Database.Persist as P
import Models (runDB)
import Revisions (saveRevision, logDeletion)

import Actions (assertTrait)

create :: Trait -> Entity User -> Action (Entity Trait)
create t u = do
  trait <- assertTrait t
  saveRevision u trait
  return trait

update :: TraitId -> Trait -> Entity User -> Action (Entity Trait)
update _id t u = do
  -- TODO: disallow logic changes
  runDB $ P.replace _id t
  let trait = Entity _id t
  saveRevision u trait
  return trait

delete :: TraitId -> Entity User -> Action (Entity Trait)
delete _id u = do
  error "delete trait"
