module Handler.Spaces
( getSpacesR
, postSpacesR
, getSpaceR
, putSpaceR
, deleteSpaceR
) where

import Import
import Control.Monad ((>=>))
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Network.HTTP.Types


import Form.Spaces
import Handler.Helpers hiding (render)
import Handler.Partials (filteredTraits, revisionList)
import Models
import Presenter.Trait (traitNameAtom)


sendError status = sendResponseStatus status $ object [ "error" .= (decodeUtf8 . statusMessage $ status) ]

requireUser :: Handler (Entity User)
requireUser = do
  let unauthed = sendError unauthorized401
  authHeader <- lookupHeader "Authorization"
  case authHeader of
    Nothing -> unauthed
    -- FIXME! - secure this auth!
    Just token -> do
      muser <- runDB . getBy . UniqueUser . decodeUtf8 $ token
      case muser of
        Nothing -> unauthed
        Just user -> return user

requireAdmin :: Handler (Entity User)
requireAdmin = do
  user <- requireUser
  case userAdmin . entityVal $ user of
    True  -> return user
    False -> sendError forbidden403

invalid422 = Status 422 "Invalid"

requireGetParam key = do
  val <- lookupGetParam key
  case val of
    Nothing  -> sendError invalid422
    Just val -> return val


getSpacesR :: Handler Value
getSpacesR = paged' [] [Asc SpaceName] >>= returnJson

postSpacesR :: Handler Value
postSpacesR = do
  _ <- requireUser
  -- FIXME: validations / forms!
  name <- requireGetParam "name"
  description <- requireGetParam "description"
  now <- liftIO getCurrentTime
  let space = Space name (Textarea description) now now Nothing
  _id <- runDB $ insert space
  returnJson $ Entity _id space

getSpaceR :: SpaceId -> Handler Value
getSpaceR = (runDB . get404) >=> returnJson

putSpaceR :: SpaceId -> Handler Value
putSpaceR _ = do
  _ <- requireAdmin
  undefined

deleteSpaceR :: SpaceId -> Handler Value
deleteSpaceR _ = do
  _ <- requireAdmin
  undefined


--getCreateSpaceR :: Handler Html
--getCreateSpaceR = do
--  (widget, enctype) <- generateFormPost createSpaceForm
--  render "New Space" $(widgetFile "spaces/new")
--
--postCreateSpaceR :: Handler Html
--postCreateSpaceR = do
--  ((result, widget), enctype) <- runFormPost createSpaceForm
--  case result of
--    FormSuccess space -> do
--      _id <- runDB $ insert space
--      _ <- revisionCreate $ Entity _id space
--      flash Success "Created space"
--      redirect $ SpaceR _id
--    _ -> render "New Space" $(widgetFile "spaces/new")
--
--getEditSpaceR :: SpaceId -> Handler Html
--getEditSpaceR _id = do
--  space <- runDB $ get404 _id
--  (widget, enctype) <- generateFormPost $ updateSpaceForm space
--  render ("Edit " <> spaceName space) $(widgetFile "spaces/edit")

--postSpaceR :: SpaceId -> Handler Html
--postSpaceR _id = do
--  space <- runDB $ get404 _id
--  ((result, widget), enctype) <- runFormPost $ updateSpaceForm space
--  case result of
--    FormSuccess updated -> do
--      runDB $ replace _id updated
--      _ <- revisionCreate $ Entity _id updated
--      flash Success "Updated space"
--      redirect $ SpaceR _id
--    _ -> render ("Edit " <> spaceName space) $(widgetFile "spaces/edit")

--getDeleteSpaceR :: SpaceId -> Handler Html
--getDeleteSpaceR _id = do
--  space <- runDB $ get404 _id
--  traits <- runDB $ count [TraitSpaceId ==. _id]
--  render ("Delete" <> spaceName space)  $(widgetFile "spaces/delete")
--
--postDeleteSpaceR :: SpaceId -> Handler Html
--postDeleteSpaceR _id = do
--  _ <- spaceDelete _id
--  flash Warning "Deleted space"
--  redirect SpacesR
--
--getSpaceRevisionsR :: SpaceId -> Handler Html
--getSpaceRevisionsR _id = do
--  space <- runDB $ get404 _id
--  render (spaceName space <> " Revisions") $(widgetFile "spaces/revisions")
