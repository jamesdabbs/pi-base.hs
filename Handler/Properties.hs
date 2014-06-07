module Handler.Properties where

import Import
import Data.Text (intercalate)

import Form.Properties
import Handler.Helpers
import Handler.Partials (filteredTraits, revisionList, theoremName)
import Models


propertyTheoremsWidget :: PropertyId -> Widget
propertyTheoremsWidget _id = do
  theorems <- handlerToWidget $ propertyTheorems _id
  $(widgetFile "properties/_theorems")

propertyTraitName :: (Entity Space, Entity Trait, Entity Property) -> Text
propertyTraitName ((Entity _ s), (Entity _ t), _) = if traitValueBool t
  then spaceName s <> " = True"
  else spaceName s <> " = False"

getPropertiesR :: Handler Html
getPropertiesR = do
  (properties, pageWidget) <- paged 10 [] [Asc PropertyName]
  total <- runDB $ count ([] :: [Filter Property])
  render "Properties" $(widgetFile "properties/index")

getPropertiesNamesR :: Handler Value
getPropertiesNamesR = do
  properties <- runDB $ selectList [] []
  returnJson . object . concat . map pNameMap $ properties
  where pNameMap (Entity _id p) = map (\name -> (name, toJSON _id)) $ propertyNames p

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
      _ <- revisionCreate $ Entity _id property
      flash Success "Created property"
      redirect $ PropertyR _id
    _ -> render "New Property" $(widgetFile "properties/new")


getEditPropertyR :: PropertyId -> Handler Html
getEditPropertyR _id = do
  property <- runDB $ get404 _id
  (widget, enctype) <- generateFormPost $ updatePropertyForm property
  render ("Edit " <> propertyName property) $(widgetFile "properties/edit")

postPropertyR :: PropertyId -> Handler Html
postPropertyR _id = do
  property <- runDB $ get404 _id
  ((result, widget), enctype) <- runFormPost $ updatePropertyForm property
  case result of
    FormSuccess updated -> do
      runDB $ replace _id updated
      _ <- revisionCreate $ Entity _id updated
      flash Success "Updated property"
      redirect $ PropertyR _id
    _ -> render ("Edit " <> propertyName property) $(widgetFile "properties/edit")

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
  _ <- propertyDelete _id
  flash Warning "Deleted property"
  redirect PropertiesR

getPropertyRevisionsR :: PropertyId -> Handler Html
getPropertyRevisionsR _id = do
  property <- runDB $ get404 _id
  render (propertyName property <> " Revisions") $(widgetFile "properties/revisions")
