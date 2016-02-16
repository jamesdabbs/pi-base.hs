module Models.Property
  ( create
  , update
  , delete
  ) where

import Base
import qualified Database.Persist as P
import Models (runDB)
import Revisions (saveRevision, logDeletion)

create :: Property -> Entity User -> Action (Entity Property)
create p u = do
  _id <- runDB $ P.insert p
  let prop = Entity _id p
  saveRevision u prop
  return prop

update :: PropertyId -> Property -> Entity User -> Action (Entity Property)
update _id p u = do
  runDB $ P.replace _id p
  let prop = Entity _id p
  saveRevision u prop
  return prop

delete :: PropertyId -> Entity User -> Action (Entity Property)
delete _id u = do
  error "delete prop" -- need get404 here, but don't want to invert dependency
  -- prop <- get404 _id
  -- -- TODO: need to delete associated traits, proofs, assumptions &c. as well
  -- runDB $ P.delete _id
  -- logDeletion u prop
  -- return prop
