module Model.Revision
( Revisable
, revisionCreate
, revisions
) where

import Import
import Yesod.Auth (requireAuthId)

import Data.Time (getCurrentTime)
import Util (encodeText)

class ToJSON a => Revisable a where
  tableName :: a -> Text

instance Revisable Property where
  tableName _ = "Property"

instance Revisable Space where
  tableName _ = "Space"

instance Revisable Trait where
  tableName _ = "Trait"

instance Revisable Theorem where
  tableName _ = "Theorem"

keyToInt64 :: Key a -> Int64
keyToInt64 (Key (PersistInt64 i)) = i
keyToInt64 _ = error "Can't coerce key to an integer"

revisionFilters :: (Revisable a) => Entity a -> [Filter Revision]
revisionFilters (Entity _id o) =
  [(RevisionItemClass ==. tableName o), (RevisionItemId ==. keyToInt64 _id)]

revisions :: (Revisable a) => Entity a -> Handler [Entity Revision]
revisions e = runDB $ selectList (revisionFilters e) [Desc RevisionCreatedAt]

revisionCreate :: (Revisable a) => Entity a -> Handler RevisionId
revisionCreate (Entity _id obj) = do
  auth <- requireAuthId
  now  <- liftIO getCurrentTime
  runDB . insert $ Revision
    { revisionItemId = keyToInt64 _id
    , revisionItemClass = tableName obj
    , revisionBody = encodeText obj
    , revisionUserId = auth
    , revisionCreatedAt = now
    }
