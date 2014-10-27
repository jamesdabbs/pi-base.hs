module Handler.Spaces
( getSpacesR
, postSpacesR
, getSpaceR
, putSpaceR
, deleteSpaceR
) where

import Import
import Control.Monad ((>=>))
import Data.Time (getCurrentTime, UTCTime)


import Form (runJsonForm)
import Handler.Helpers (paged', requireUser, requireAdmin)
import Models


createSpaceForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => UTCTime -> FormInput m Space
createSpaceForm now = Space
  <$> ireq textField "name"
  <*> ireq textareaField "description"
  <*> pure now
  <*> pure now
  <*> iopt textareaField "proof_of_topology"

updateSpaceForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => Space -> UTCTime -> FormInput m Space
updateSpaceForm s now = Space
  <$> pure (spaceName s)
  <*> ireq textareaField "description"
  <*> pure (spaceCreatedAt s)
  <*> pure now
  <*> iopt textareaField "proof_of_topology"


getSpacesR :: Handler Value
getSpacesR = paged' [] [Asc SpaceName] >>= returnJson

postSpacesR :: Handler Value
postSpacesR = do
  _ <- requireUser
  now <- lift getCurrentTime
  space <- runJsonForm $ createSpaceForm now
  _id <- runDB $ insert space
  returnJson $ Entity _id space

getSpaceR :: SpaceId -> Handler Value
getSpaceR = (runDB . get404) >=> returnJson

putSpaceR :: SpaceId -> Handler Value
putSpaceR _id = do
  _ <- requireAdmin
  space <- runDB $ get404 _id
  now <- liftIO getCurrentTime
  updated <- runJsonForm $ updateSpaceForm space now
  runDB $ replace _id updated
  -- TODO: revision tracking
  returnJson $ Entity _id updated

deleteSpaceR :: SpaceId -> Handler Value
deleteSpaceR _id = do
  _ <- requireAdmin
  space <- runDB $ get404 _id
  _ <- spaceDelete _id
  returnJson $ Entity _id space

-- TODO: delete preview
--getDeleteSpaceR :: SpaceId -> Handler Html
--getDeleteSpaceR _id = do
--  space <- runDB $ get404 _id
--  traits <- runDB $ count [TraitSpaceId ==. _id]
--  render ("Delete" <> spaceName space)  $(widgetFile "spaces/delete")

-- TODO: index revisions
--getSpaceRevisionsR :: SpaceId -> Handler Html
--getSpaceRevisionsR _id = do
--  space <- runDB $ get404 _id
--  render (spaceName space <> " Revisions") $(widgetFile "spaces/revisions")
