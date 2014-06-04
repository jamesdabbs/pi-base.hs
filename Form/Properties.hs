module Form.Properties
( createPropertyForm
, updatePropertyForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

-- FIXME: query instead of hardcoding
boolean :: Handler ValueSetId
boolean = do
  mid <- runDB . getBy $ UValueSetName "boolean"
  case mid of
    Just (Entity _id _) -> return _id
    Nothing -> error "Could not find expected 'boolean' value set"

createPropertyForm :: Html -> MForm Handler (FormResult Property, Widget)
createPropertyForm = renderBootstrap3 fLayout $ Property
  <$> areq textField (fs "Name") Nothing
  <*> areq textareaField (fs "Description") Nothing
  <*> lift (boolean)
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*  save

updatePropertyForm :: Property -> Html -> MForm Handler (FormResult Property, Widget)
updatePropertyForm p = renderBootstrap3 fLayout $ Property
  <$> pure (propertyName p)
  <*> areq textareaField (fs "Description") (Just $ propertyDescription p)
  <*> pure (propertyValueSetId p)
  <*> pure (propertyCreatedAt p)
  <*> lift (liftIO getCurrentTime)
  <*  save
