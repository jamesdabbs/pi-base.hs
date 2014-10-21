module Handler.Spaces
( getSpacesR
, postSpacesR
, getSpaceR
, putSpaceR
, deleteSpaceR
) where

import Import
import Control.Monad ((>=>))
import Data.Time (getCurrentTime)


import Handler.Helpers (paged', requireUser, requireAdmin, requireGetParam)
import Models


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
