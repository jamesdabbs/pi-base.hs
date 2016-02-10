{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Revisions
  ( Revisable
  , revisionsFor
  ) where

import Base

import Data.Aeson
import Database.Persist

class PersistEntity a => Revisable a where
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

idToInt :: PersistEntity a => Key a -> Int64
idToInt k = case keyToValues k of
  [PersistInt64 n] -> n
  _ -> error "Can't coerce key to a single integer"

revisionsFor :: Revisable a => Key a -> [Filter Revision]
revisionsFor _id = [RevisionItemId ==. idToInt _id, RevisionItemClass ==. revisionClassName _id]
