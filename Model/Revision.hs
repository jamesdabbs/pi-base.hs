module Model.Revision
( Revisable
, revisionCreate
, revisions
, logDeletion
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

-- FIXME
keyToInt64 = read . show

revisionFilters :: (Revisable a) => Entity a -> [Filter Revision]
revisionFilters (Entity _id o) =
  [(RevisionItemClass ==. tableName o), (RevisionItemId ==. keyToInt64 _id)]

revisions :: (Revisable a) => Entity a -> Handler [Entity Revision]
revisions e = runDB $ selectList (revisionFilters e) [Desc RevisionCreatedAt]

revisionCreate' :: (Revisable a) => Bool -> Entity a -> Handler RevisionId
revisionCreate' del (Entity _id obj) = do
  auth <- requireAuthId
  now  <- liftIO getCurrentTime
  runDB . insert $ Revision
    { revisionItemId = keyToInt64 _id
    , revisionItemClass = tableName obj
    , revisionBody = encodeText obj
    , revisionUserId = auth
    , revisionCreatedAt = now
    , revisionDeletes = del
    }

revisionCreate :: (Revisable a) => Entity a -> Handler RevisionId
revisionCreate = revisionCreate' False

logDeletion :: (Revisable a) => Entity a -> Handler ()
logDeletion e = do
  _ <- revisionCreate' True e
  return ()
