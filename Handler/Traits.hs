module Handler.Traits
( getTraitsR
, getSpaceTraitsR
, postTraitsR
, getTraitR
, putTraitR
, deleteTraitR
, getTraitRevisionsR
) where

import Import

import qualified Data.Text as T
import Database.Persist.Sql as SQL

import Explore (async, checkTrait, checkSpace)
import Form (runJsonForm)
import qualified Handler.Base as H
import Handler.Helpers
import Logic (boolToValueId, valueIdToBool)
import Models

idField :: (YesodPersist site, PersistEntity val,
            PersistStore (PersistEntityBackend val),
            YesodPersistBackend site ~ PersistEntityBackend val) =>
           (BackendKey SqlBackend -> Key val)
           -> Field (HandlerT site IO) (Key val)
idField keyFunc = Field
  { fieldParse = \rawVals _fileVals -> case rawVals of
      [_id] -> do
        -- TODO: don't use read
        let key = keyFunc . SQL.SqlBackendKey . read . T.unpack $ _id
        ms <- runDB . get $ key
        case ms of
          Just _ -> return . Right $ Just key
          _ -> return $ Left "No object with given id"
      _ -> return $ Left "Must provide one id"
  , fieldView = undefined
  , fieldEnctype = undefined
  }

spaceIdField :: Field Handler SpaceId
spaceIdField = idField SpaceKey

propertyIdField :: Field Handler PropertyId
propertyIdField = idField PropertyKey

createTraitForm :: FormInput Handler TraitCreateData
createTraitForm = TraitCreateData
  <$> ireq spaceIdField "space_id"
  <*> ireq propertyIdField "property_id"
  <*> (boolToValueId <$> ireq boolField "value")
  <*> ireq textareaField "description"


showTrait :: Entity Trait -> Value
showTrait (Entity _id t) = object
  [ "id"         .= _id
  , "property_id".= traitPropertyId t
  , "space_id"   .= traitSpaceId t
  , "value"      .= (valueIdToBool $ traitValueId t)
  , "description".= traitDescription t
  , "deduced"    .= traitDeduced t
  ]

getSpaceTraitsR :: SpaceId -> Handler Value
getSpaceTraitsR _sid = H.index' [TraitSpaceId ==. _sid] [Desc TraitUpdatedAt] showTrait

getTraitsR :: Handler Value
getTraitsR = H.index [Desc TraitUpdatedAt] showTrait

postTraitsR :: Handler Value
postTraitsR = do
  _ <- requireUser
  d <- runJsonForm createTraitForm
  e@(Entity _id _) <- traitCreate d
  async checkTrait _id
  returnJson . showTrait $ e

-- TODO: show deduced, supports, etc
getTraitR :: TraitId -> Handler Value
getTraitR = H.show id

putTraitR :: TraitId -> Handler Value
putTraitR = H.update updateForm traitUpdate showTrait
  where
    updateForm = TraitUpdateData
      <$> ireq textareaField "description"

deleteTraitR :: TraitId -> Handler Value
deleteTraitR _id = do
  _ <- requireAdmin
  t <- runDB $ get404 _id
  _ <- traitDelete _id
  async checkSpace $ traitSpaceId t
  returnJson . showTrait $ Entity _id t

getTraitRevisionsR :: TraitId -> Handler Value
getTraitRevisionsR = H.revisions
