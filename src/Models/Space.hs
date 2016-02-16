module Models.Space
  ( create
  , update
  , delete
  ) where

import Base
import qualified Database.Persist as P
import Models (runDB)
import Revisions (saveRevision, logDeletion)

create :: Space -> Entity User -> Action (Entity Space)
create s u = do
  _id <- runDB $ P.insert s
  let space = Entity _id s
  saveRevision u space
  return space

update :: SpaceId -> Space -> Entity User -> Action (Entity Space)
update _id s u = do
  runDB $ P.replace _id s
  let space = Entity _id s
  saveRevision u space
  return space

delete :: SpaceId -> Entity User -> Action (Entity Space)
delete _id u = do
  error "delete space" -- need get404 here, but don't want to invert dependency
  -- space <- get404 _id
  -- -- TODO: need to delete associated traits, proofs, assumptions &c. as well
  -- runDB $ P.delete _id
  -- logDeletion u space
  -- return space
