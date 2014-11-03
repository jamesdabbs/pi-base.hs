module Model.Property
( propertyTheorems
, PropertyCreateData (..)
, propertyCreate
, PropertyUpdateData (..)
, propertyUpdate
, propertyDelete
, propertyNames
) where

import Import hiding ((==.))
import qualified Import as I ((==.))

import Database.Esqueleto hiding (delete)

import Model.Revision

getBoolean :: Handler ValueSetId
getBoolean = do
  mb <- runDB . getBy $ UValueSetName "boolean"
  case mb of
    Just (Entity _id _) -> return _id
    Nothing -> do
      now <- liftIO getCurrentTime
      runDB . insert $ ValueSet "boolean" now now

data PropertyCreateData = PropertyCreateData
  { pcdName :: Text
  , pcdDescription :: Textarea
  }

data PropertyUpdateData = PropertyUpdateData
  { pudDescription :: Textarea
  }

propertyCreate :: PropertyCreateData -> Handler (Entity Property)
propertyCreate d = do
  bool <- getBoolean
  now <- lift getCurrentTime
  let property = Property (pcdName d) [] (pcdDescription d) bool now now
  createWithRevision property

propertyUpdate :: Entity Property -> PropertyUpdateData -> Handler (Entity Property)
propertyUpdate (Entity _id p) d = do
  now <- lift getCurrentTime
  let updated = p { propertyDescription = pudDescription d, propertyUpdatedAt = now }
  updateWithRevision _id updated

propertyTheorems :: PropertyId -> Handler [Entity Theorem]
propertyTheorems pid = runDB . select $
  from $ \(t `InnerJoin` pt) -> do
  on (t ^. TheoremId ==. pt ^. TheoremPropertyTheoremId)
  where_ (pt ^. TheoremPropertyPropertyId ==. val pid)
  return t

propertyDelete :: PropertyId -> Handler ()
propertyDelete _id = do
  property <- runDB $ get404 _id
  runDB $ deleteWhere [TraitPropertyId I.==. _id]
  deleteWithRevision _id property

propertyNames :: Property -> [Text]
propertyNames p = propertyName p : propertyAliases p
