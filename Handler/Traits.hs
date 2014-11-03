module Handler.Traits where

import Import

import Control.Monad ((>=>))
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql as SQL

import DB (derivedTraits)
import Explore (async, checkTrait, checkSpace)
import Form (runJsonForm)
import Handler.Partials (revisionList)
import Handler.Helpers
import Logic (boolToValueId)
import Models


idField keyFunc = Field
  { fieldParse = \rawVals _fileVals -> case rawVals of
      [id] -> do
        -- TODO: don't use read
        let key = keyFunc . SQL.SqlBackendKey . read . T.unpack $ id
        ms <- runDB . get $ key
        case ms of
          Just _ -> return . Right $ Just key
          _ -> return $ Left "No object with given id"
      _ -> return $ Left "Must provide one id"
  , fieldView = undefined
  , fieldEnctype = undefined
  }

spaceIdField = idField SpaceKey
propertyIdField = idField PropertyKey


createTraitForm now = Trait
  <$> ireq spaceIdField "space_id"
  <*> ireq propertyIdField "property_id"
  <*> (boolToValueId <$> ireq boolField "value")
  <*> ireq textareaField "description"
  <*> pure now
  <*> pure now
  <*> pure False

updateTraitForm t now = Trait
  <$> pure (traitSpaceId t)
  <*> pure (traitPropertyId t)
  <*> pure (traitValueId t)
  <*> ireq textareaField "description"
  <*> pure (traitCreatedAt t)
  <*> pure now
  <*> pure (traitDeduced t)


getSpaceTraitsR :: SpaceId -> Handler Value
getSpaceTraitsR _sid = paged' [TraitSpaceId ==. _sid] [Desc TraitUpdatedAt] >>= returnJson

getTraitsR :: Handler Value
getTraitsR = paged' [] [Desc TraitUpdatedAt] >>= returnJson

postTraitsR :: Handler Value
postTraitsR = do
  _ <- requireUser
  now <- lift getCurrentTime
  pid <- lookupPostParam "property_id"
  $(logInfo) $ "property_id - " <> (T.pack . show $ pid)
  trait <- runJsonForm $ createTraitForm now
  $(logInfo) $ "Property Id - " <> (T.pack . show . traitPropertyId $ trait)
  existing <- runDB . getBy $ TraitSP (traitSpaceId trait) (traitPropertyId trait)
  case existing of
    Just trait -> do
      error "Should render 422"
    Nothing -> do
      _id <- runDB $ insert trait
      -- FIXME: _ <- revisionCreate $ Entity _id trait
      async checkTrait _id
      returnJson $ Entity _id trait

-- TODO: show deduced, supports, etc
getTraitR :: TraitId -> Handler Value
getTraitR = (runDB . get404) >=> returnJson

putTraitR :: TraitId -> Handler Value
putTraitR _id = do
  _ <- requireAdmin
  trait <- runDB $ get404 _id
  now <- liftIO getCurrentTime
  updated <- runJsonForm $ updateTraitForm trait now
  runDB $ replace _id updated
  returnJson $ Entity _id updated

deleteTraitR :: TraitId -> Handler Value
deleteTraitR _id = do
  _ <- requireAdmin
  trait <- runDB $ get404 _id
  _ <- traitDelete _id
  returnJson $ Entity _id trait
