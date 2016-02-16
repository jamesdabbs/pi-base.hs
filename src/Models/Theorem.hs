{-# LANGUAGE OverloadedStrings #-}
 module Models.Theorem
  ( create
  , update
  , delete
  ) where

import Base
import qualified Database.Persist as P
import Models (runDB)
import Revisions (saveRevision, logDeletion)

import Actions (assertTheorem)

create :: Theorem -> Entity User -> Action (Entity Theorem)
create t u = do
  -- TODO: what if the assertion is disprovable?
  thrm <- assertTheorem $ error "implication to theorem" $ t
  saveRevision u thrm
  return thrm

update :: TheoremId -> Theorem -> Entity User -> Action (Entity Theorem)
update t u = do
  -- TODO: disallow logic changes
  error "update theorem"

delete :: TheoremId -> Entity User -> Action (Entity Theorem)
delete _id u = do
  error "delete theorem"
