module Handler.Properties where

import Import

import Form.Properties (createPropertyForm)
import Handler.Helpers
import Handler.Partials (filteredTraits)


getPropertiesR :: Handler Html
getPropertiesR = do
  (properties, pageWidget) <- page 10
  total <- runDB $ count ([] :: [Filter Property])
  defaultLayout $(widgetFile "properties/index")

getCreatePropertyR :: Handler Html
getCreatePropertyR = do
  (widget, enctype) <- generateFormPost createPropertyForm
  defaultLayout $(widgetFile "properties/new")

postCreatePropertyR :: Handler Html
postCreatePropertyR = do
  ((result, widget), enctype) <- runFormPost createPropertyForm
  case result of
    FormSuccess property -> do
      _id <- runDB $ insert property
      setMessage "Created property"
      redirect $ PropertyR _id
    _ -> defaultLayout $(widgetFile "properties/new")

getPropertyR :: PropertyId -> Handler Html
getPropertyR _id = do
  property <- runDB $ get404 _id
  defaultLayout $(widgetFile "properties/show")

getDeletePropertyR :: PropertyId -> Handler Html
getDeletePropertyR _id = do
  property <- runDB $ get404 _id
  traits <- runDB $ count [TraitPropertyId ==. _id]
  defaultLayout $(widgetFile "properties/delete")

postDeletePropertyR :: PropertyId -> Handler Html
postDeletePropertyR _id = do
  runDB $ deleteWhere [TraitPropertyId ==. _id]
  runDB $ delete _id
  setMessage "Deleted property"
  redirect PropertiesR
