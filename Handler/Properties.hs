module Handler.Properties where

import Import

import Form.Properties (createPropertyForm)
import Handler.Helpers
import Handler.Partials (filteredTraits)


getPropertiesR :: Handler Html
getPropertiesR = do
  (properties, pageWidget) <- page 10
  total <- runDB $ count ([] :: [Filter Property])
  render "Properties" $(widgetFile "properties/index")

getCreatePropertyR :: Handler Html
getCreatePropertyR = do
  (widget, enctype) <- generateFormPost createPropertyForm
  render "New Property" $(widgetFile "properties/new")

postCreatePropertyR :: Handler Html
postCreatePropertyR = do
  ((result, widget), enctype) <- runFormPost createPropertyForm
  case result of
    FormSuccess property -> do
      _id <- runDB $ insert property
      setMessage "Created property"
      redirect $ PropertyR _id
    _ -> render "New Property" $(widgetFile "properties/new")

getPropertyR :: PropertyId -> Handler Html
getPropertyR _id = do
  property <- runDB $ get404 _id
  render (propertyName property) $(widgetFile "properties/show")

getDeletePropertyR :: PropertyId -> Handler Html
getDeletePropertyR _id = do
  property <- runDB $ get404 _id
  traits <- runDB $ count [TraitPropertyId ==. _id]
  render ("Delete " <> propertyName property) $(widgetFile "properties/delete")

postDeletePropertyR :: PropertyId -> Handler Html
postDeletePropertyR _id = do
  runDB $ deleteWhere [TraitPropertyId ==. _id]
  runDB $ delete _id
  setMessage "Deleted property"
  redirect PropertiesR
