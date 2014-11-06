module Model.Revision
( Revisable
, createWithRevision
, updateWithRevision
, deleteWithRevision
, revisions
) where

import Import
import Database.Persist.Sql (SqlBackend)

import Util (encodeText)

import Handler.Helpers (requireUser)


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


createWithRevision :: (Revisable a, PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
                      a -> Handler (Entity a)
createWithRevision val = do
  _id <- runDB $ insert val
  let e = Entity _id val
  _ <- revisionCreate e
  return e

updateWithRevision :: (Revisable a, PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
                      Key a -> a -> Handler (Entity a)
updateWithRevision _id val = do
  runDB $ replace _id val
  let e = Entity _id val
  _ <- revisionCreate e
  return e

deleteWithRevision :: (Revisable a, PersistEntity a, PersistEntityBackend a ~ SqlBackend) =>
                      Key a -> a -> Handler ()
deleteWithRevision _id val = do
  logDeletion $ Entity _id val
  runDB $ delete _id


keyToInt64 :: (PersistEntity a) => Key a -> Int64
keyToInt64 key = case keyToValues key of
  [PersistInt64 _id] -> _id
  _ -> error "Can't extract integer key"

revisionFilters :: (Revisable a) => Entity a -> [Filter Revision]
revisionFilters (Entity _id o) =
  [(RevisionItemClass ==. tableName o), (RevisionItemId ==. keyToInt64 _id)]

revisions :: (Revisable a) => Entity a -> Handler [Entity Revision]
revisions e = runDB $ selectList (revisionFilters e) [Desc RevisionCreatedAt]

revisionCreate' :: (Revisable a) => Bool -> Entity a -> Handler RevisionId
revisionCreate' del (Entity _id obj) = do
  user <- requireUser
  now  <- liftIO getCurrentTime
  $(logInfo) "Creating revision"
  runDB . insert $ Revision
    { revisionItemId = keyToInt64 _id
    , revisionItemClass = tableName obj
    , revisionBody = encodeText obj
    , revisionUserId = entityKey user
    , revisionCreatedAt = now
    , revisionDeletes = del
    }

revisionCreate :: (Revisable a) => Entity a -> Handler RevisionId
revisionCreate = revisionCreate' False

logDeletion :: (Revisable a) => Entity a -> Handler ()
logDeletion e = do
  _ <- revisionCreate' True e
  return ()
