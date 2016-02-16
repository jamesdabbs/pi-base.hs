{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Revisions
  ( Revisable
  , revisionsFor
  , saveRevision
  , logDeletion
  ) where

import Base
import Models (runDB)
import Pager (pageJSON)

import Data.Aeson
import Database.Persist
import Database.Persist.Sql

class (PersistEntity a, ToBackendKey SqlBackend a) => Revisable a where
  revisionClassName :: Key a -> Text

instance Revisable Property where
  revisionClassName _ = "Property"

instance Revisable Space where
  revisionClassName _ = "Space"

instance Revisable Theorem where
  revisionClassName _ = "Theorem"

instance Revisable Trait where
  revisionClassName _ = "Trait"

instance ToJSON (Page Revision) where
  toJSON = pageJSON "revisions" $
    \(Entity _id Revision{..}) -> object
      [ "id"         .= _id
      , "user_id"    .= revisionUserId
      , "body"       .= revisionBody
      , "created_at" .= revisionCreatedAt
      ]

instance ToJSON Revision where
  toJSON = error "ToJSON Revision"

revisionsFor :: Revisable a => Key a -> [Filter Revision]
revisionsFor _id = [RevisionItemId ==. fromSqlKey _id, RevisionItemClass ==. revisionClassName _id]

saveRevision :: Revisable a => Entity User -> Entity a -> Action ()
saveRevision (Entity userId _) obj@(Entity _id _) =
  void . runDB . insert $ Revision
    { revisionItemId    = fromSqlKey _id
    , revisionItemClass = revisionClassName _id
    -- FIXME: the ToJSON instances for these objects have changed,
    --   but the revision bodies need to be backwards compatible
    , revisionBody      = ""
    , revisionUserId    = userId
    , revisionCreatedAt = Nothing
    , revisionDeletes   = False
    }

logDeletion :: Revisable a => Entity User -> Entity a -> Action ()
logDeletion = error "logDeletion"
