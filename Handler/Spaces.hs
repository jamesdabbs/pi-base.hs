module Handler.Spaces where

import Import
import Control.Monad ((>=>))


import Form.Spaces
import Handler.Helpers
import Handler.Partials (filteredTraits, revisionList)
import Models
import Presenter.Trait (traitNameAtom)



getSpacesR :: Handler Value
getSpacesR = paged' [] [Asc SpaceName] >>= returnJson

getSpaceR :: SpaceId -> Handler Value
getSpaceR = (runDB . get404) >=> returnJson


getCreateSpaceR :: Handler Html
getCreateSpaceR = do
  (widget, enctype) <- generateFormPost createSpaceForm
  render "New Space" $(widgetFile "spaces/new")

postCreateSpaceR :: Handler Html
postCreateSpaceR = do
  ((result, widget), enctype) <- runFormPost createSpaceForm
  case result of
    FormSuccess space -> do
      _id <- runDB $ insert space
      _ <- revisionCreate $ Entity _id space
      flash Success "Created space"
      redirect $ SpaceR _id
    _ -> render "New Space" $(widgetFile "spaces/new")

getEditSpaceR :: SpaceId -> Handler Html
getEditSpaceR _id = do
  space <- runDB $ get404 _id
  (widget, enctype) <- generateFormPost $ updateSpaceForm space
  render ("Edit " <> spaceName space) $(widgetFile "spaces/edit")

postSpaceR :: SpaceId -> Handler Html
postSpaceR _id = do
  space <- runDB $ get404 _id
  ((result, widget), enctype) <- runFormPost $ updateSpaceForm space
  case result of
    FormSuccess updated -> do
      runDB $ replace _id updated
      _ <- revisionCreate $ Entity _id updated
      flash Success "Updated space"
      redirect $ SpaceR _id
    _ -> render ("Edit " <> spaceName space) $(widgetFile "spaces/edit")

getDeleteSpaceR :: SpaceId -> Handler Html
getDeleteSpaceR _id = do
  space <- runDB $ get404 _id
  traits <- runDB $ count [TraitSpaceId ==. _id]
  render ("Delete" <> spaceName space)  $(widgetFile "spaces/delete")

postDeleteSpaceR :: SpaceId -> Handler Html
postDeleteSpaceR _id = do
  _ <- spaceDelete _id
  flash Warning "Deleted space"
  redirect SpacesR

getSpaceRevisionsR :: SpaceId -> Handler Html
getSpaceRevisionsR _id = do
  space <- runDB $ get404 _id
  render (spaceName space <> " Revisions") $(widgetFile "spaces/revisions")
