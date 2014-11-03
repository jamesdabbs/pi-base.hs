module Handler.Spaces
( getSpacesR
, postSpacesR
, getSpaceR
, putSpaceR
, deleteSpaceR
, getSpaceRevisionsR
) where

import Import

import qualified Handler.Base as H
import Models


getSpacesR :: Handler Value
getSpacesR = H.index [Asc SpaceName] id

postSpacesR :: Handler Value
postSpacesR = H.create createForm spaceCreate id
  where
    createForm = SpaceCreateData
      <$> ireq textField "name"
      <*> ireq textareaField "description"
      <*> iopt textareaField "proof_of_topology"

getSpaceR :: SpaceId -> Handler Value
getSpaceR = H.show id

putSpaceR :: SpaceId -> Handler Value
putSpaceR = H.update updateForm spaceUpdate id
  where
    updateForm = SpaceUpdateData
      <$> ireq textareaField "description"
      <*> iopt textareaField "proof_of_topology"

deleteSpaceR :: SpaceId -> Handler Value
deleteSpaceR = H.delete spaceDelete id

getSpaceRevisionsR :: SpaceId -> Handler Value
getSpaceRevisionsR = H.revisions

-- TODO: delete preview
--getDeleteSpaceR :: SpaceId -> Handler Html
--getDeleteSpaceR _id = do
--  space <- runDB $ get404 _id
--  traits <- runDB $ count [TraitSpaceId ==. _id]
--  render ("Delete" <> spaceName space)  $(widgetFile "spaces/delete")
