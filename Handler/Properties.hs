module Handler.Properties
( getPropertiesR
, postPropertiesR
, getPropertyR
, putPropertyR
, deletePropertyR
, getPropertyRevisionsR
) where

import Import

import qualified Handler.Base as H
import Models

presenter :: Entity Property -> Value
presenter (Entity _id p) = object
  [ "id"          .= _id
  , "name"        .= propertyName p
  , "description" .= propertyDescription p
  , "value_set_id".= propertyValueSetId p
  ]

getPropertiesR :: Handler Value
getPropertiesR = H.index "properties" [Asc PropertyName] presenter

postPropertiesR :: Handler Value
postPropertiesR = H.create createForm propertyCreate presenter
  where
    createForm = PropertyCreateData
      <$> ireq textField "name"
      <*> ireq textareaField "description"

-- FIXME: add theorems, aliases, properties? to JSON response
getPropertyR :: PropertyId -> Handler Value
getPropertyR = H.show presenter

putPropertyR :: PropertyId -> Handler Value
putPropertyR = H.update updateForm propertyUpdate presenter
  where
    updateForm = PropertyUpdateData
      <$> ireq textareaField "description"

deletePropertyR :: PropertyId -> Handler Value
deletePropertyR = H.delete propertyDelete presenter

getPropertyRevisionsR :: PropertyId -> Handler Value
getPropertyRevisionsR = H.revisions
