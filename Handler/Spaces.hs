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

presenter :: Entity Space -> Value
presenter (Entity _id s) = object
  [ "id"               .= _id
  , "name"             .= spaceName s
  , "description"      .= spaceDescription s
  , "proof_of_topology".= spaceProofOfTopology s
  ]

getSpacesR :: Handler Value
getSpacesR = H.index "spaces" [Asc SpaceName] presenter
--getSpacesR = do
--  spaces <- H.paged [] [Asc SpaceName]
--  returnJson $ object
--    [ "spaces" .= (map presenter $ spaces)
--    , "meta" .= object [ "total_pages" .= (7 :: Int) ]
--    ]

postSpacesR :: Handler Value
postSpacesR = H.create createForm spaceCreate presenter
  where
    createForm = SpaceCreateData
      <$> ireq textField "name"
      <*> ireq textareaField "description"
      <*> iopt textareaField "proof_of_topology"

getSpaceR :: SpaceId -> Handler Value
getSpaceR _id = do
  space <- runDB $ get404 _id
  returnJson $ object [ "space" .= (presenter $ Entity _id space) ]

putSpaceR :: SpaceId -> Handler Value
putSpaceR = H.update updateForm spaceUpdate presenter
  where
    updateForm = SpaceUpdateData
      <$> ireq textareaField "description"
      <*> iopt textareaField "proof_of_topology"

deleteSpaceR :: SpaceId -> Handler Value
deleteSpaceR = H.delete spaceDelete presenter

getSpaceRevisionsR :: SpaceId -> Handler Value
getSpaceRevisionsR = H.revisions

-- TODO: delete preview
--getDeleteSpaceR :: SpaceId -> Handler Html
--getDeleteSpaceR _id = do
--  space <- runDB $ get404 _id
--  traits <- runDB $ count [TraitSpaceId ==. _id]
--  render ("Delete" <> spaceName space)  $(widgetFile "spaces/delete")
