module Handler.Properties where

import Import
import Control.Monad ((>=>))
import Data.Time (getCurrentTime, UTCTime)

import Form (runJsonForm)
import Handler.Helpers
import Models


createPropertyForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => UTCTime -> ValueSetId -> FormInput m Property
createPropertyForm now valueSet = Property
  <$> ireq textField "name"
  <*> pure []
  <*> ireq textareaField "description"
  <*> pure valueSet
  <*> pure now
  <*> pure now

updatePropertyForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => Property -> UTCTime -> FormInput m Property
updatePropertyForm p now = Property
  <$> pure (propertyName p)
  <*> pure (propertyAliases p)
  <*> ireq textareaField "description"
  <*> pure (propertyValueSetId p)
  <*> pure (propertyCreatedAt p)
  <*> pure now


getBoolean :: Handler ValueSetId
getBoolean = do
  mb <- runDB . getBy $ UValueSetName "boolean"
  case mb of
    Just (Entity _id _) -> return _id
    Nothing -> do
      now <- liftIO getCurrentTime
      runDB . insert $ ValueSet "boolean" now now


getPropertiesR :: Handler Value
getPropertiesR = paged' [] [Asc PropertyName] >>= returnJson

postPropertiesR :: Handler Value
postPropertiesR = do
  _ <- requireUser
  now <- lift getCurrentTime
  boolean <- getBoolean
  property <- runJsonForm $ createPropertyForm now boolean
  _id <- runDB $ insert property
  returnJson $ Entity _id property

-- FIXME: add theorems, aliases, properties? to JSON response
getPropertyR :: PropertyId -> Handler Value
getPropertyR = (runDB . get404) >=> returnJson

putPropertyR :: PropertyId -> Handler Value
putPropertyR _id = do
  _ <- requireAdmin
  property <- runDB $ get404 _id
  now <- liftIO getCurrentTime
  updated <- runJsonForm $ updatePropertyForm property now
  runDB $ replace _id updated
  -- TODO: revision tracking
  returnJson $ Entity _id updated

deletePropertyR :: PropertyId -> Handler Value
deletePropertyR _id = do
  _ <- requireAdmin
  property <- runDB $ get404 _id
  _ <- propertyDelete _id
  returnJson $ Entity _id property
