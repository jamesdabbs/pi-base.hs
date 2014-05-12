module Handler.Spaces where

import Import

import Form.Space (createSpaceForm)
import Handler.Resource (page)


getSpacesR :: Handler Html
getSpacesR = do
  spaces <- page 0 10
  defaultLayout $(widgetFile "spaces/index")

getCreateSpaceR :: Handler Html
getCreateSpaceR = do
  (widget, enctype) <- generateFormPost createSpaceForm
  defaultLayout $(widgetFile "spaces/new")

postCreateSpaceR :: Handler Html
postCreateSpaceR = do
  ((result, widget), enctype) <- runFormPost createSpaceForm
  case result of
    FormSuccess space -> do
      _id <- runDB $ insert space
      setMessage "Created space"
      redirect $ SpaceR _id
    _ -> defaultLayout $(widgetFile "spaces/new")

getSpaceR :: SpaceId -> Handler Html
getSpaceR _id = do
  space <- runDB $ get404 _id
  defaultLayout $(widgetFile "spaces/show")

getDeleteSpaceR :: SpaceId -> Handler Html
getDeleteSpaceR _id = do
  space <- runDB $ get404 _id
  traits <- runDB $ count [TraitSpaceId ==. _id]
  defaultLayout $(widgetFile "spaces/delete")

postDeleteSpaceR :: SpaceId -> Handler Html
postDeleteSpaceR _id = do
  runDB $ deleteWhere [TraitSpaceId ==. _id]
  runDB $ delete _id
  setMessage "Deleted space"
  redirect SpacesR
