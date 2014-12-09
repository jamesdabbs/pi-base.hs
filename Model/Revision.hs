module Model.Revision
( Revisable
, revisionCreate
, revisions
, revisionLink
, revisionPrettyBody
, logDeletion
) where

import Import
import Yesod.Auth (requireAuthId)
import Yesod.Routes.Class (Route)

import Data.Aeson (decode, Object)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (getCurrentTime)
import Util (encodeText)

class ToJSON a => Revisable a where
  tableName :: a -> Text
  link :: Key a -> Route App

instance Revisable Property where
  tableName _ = "Property"
  link = PropertyR

instance Revisable Space where
  tableName _ = "Space"
  link = SpaceR

instance Revisable Trait where
  tableName _ = "Trait"
  link = TraitR

instance Revisable Theorem where
  tableName _ = "Theorem"
  link = TheoremR

revisionPrettyBody :: Revision -> Text
revisionPrettyBody r =
  let parsed = decode . encodeUtf8 . fromStrict . revisionBody $ r :: Maybe Object
  in case parsed of
    Just o -> toStrict . decodeUtf8 . encodePretty $ o
    Nothing -> revisionBody r <> " (could not parse)"

revisionLink :: Revision -> Route App
revisionLink r =
  let _id = PersistInt64 . revisionItemId $ r
  in case revisionItemClass r of
    "Space"    -> SpaceR . Key $ _id
    "Property" -> PropertyR . Key $ _id
    "Trait"    -> TraitR . Key $ _id
    "Theorem"  -> TheoremR . Key $ _id
    other      -> error $ "Cannot link to revision of type " ++ show other

keyToInt64 :: Key a -> Int64
keyToInt64 (Key (PersistInt64 i)) = i
keyToInt64 _ = error "Can't coerce key to an integer"

revisionFilters :: (Revisable a) => Entity a -> [Filter Revision]
revisionFilters (Entity _id o) =
  [RevisionItemClass ==. tableName o, RevisionItemId ==. keyToInt64 _id]

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
