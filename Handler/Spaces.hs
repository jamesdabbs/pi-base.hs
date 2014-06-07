module Handler.Spaces where

import Import

import Form.Spaces
import Handler.Helpers
import Handler.Partials (filteredTraits, revisionList)
import Models

spaceTraitName :: (Entity Space, Entity Trait, Entity Property) -> Text
spaceTraitName (_, (Entity _ t), (Entity _ p)) = if traitValueBool t
  then propertyName p
  else "~" <> propertyName p

getSpacesR :: Handler Html
getSpacesR = do
  (spaces, pager) <- paged 10 [] [Asc SpaceName]
  total <- runDB $ count ([] :: [Filter Space])
  render "Spaces" $(widgetFile "spaces/index")

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

getSpaceR :: SpaceId -> Handler Html
getSpaceR _id = do
  space <- runDB $ get404 _id
  render (spaceName space) $(widgetFile "spaces/show")

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
