module Handler.Properties where

import Import
import Control.Monad ((>=>))
import Data.Text (intercalate)

import Form.Properties
import Handler.Helpers
import Handler.Partials (revisionList)
import Models


getPropertiesR :: Handler Value
getPropertiesR = paged' [] [Asc PropertyName] >>= returnJson

-- FIXME: add theorems, aliases, properties? to JSON response
getPropertyR :: PropertyId -> Handler Value
getPropertyR = (runDB . get404) >=> returnJson


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
