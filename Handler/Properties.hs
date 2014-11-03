module Handler.Properties
( getPropertiesR
, postPropertiesR
, getPropertyR
, putPropertyR
, deletePropertyR
) where

import Import

import qualified Handler.Base as H
import Models

getPropertiesR :: Handler Value
getPropertiesR = H.index [Asc PropertyName] id

postPropertiesR :: Handler Value
postPropertiesR = H.create createForm propertyCreate id
  where
    createForm = PropertyCreateData
      <$> ireq textField "name"
      <*> ireq textareaField "description"

-- FIXME: add theorems, aliases, properties? to JSON response
getPropertyR :: PropertyId -> Handler Value
getPropertyR = H.show id

putPropertyR :: PropertyId -> Handler Value
putPropertyR = H.update updateForm propertyUpdate id
  where
    updateForm = PropertyUpdateData
      <$> ireq textareaField "description"

deletePropertyR :: PropertyId -> Handler Value
deletePropertyR = H.delete propertyDelete id
