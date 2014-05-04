module Handler.Spaces where

import Import

import Handler.Resource

getSpacesR :: Handler Value
getSpacesR = do
  spaces <- page 0 10
  returnJson $ (spaces :: [Entity Space])

getSpaceR :: SpaceId -> Handler Value
getSpaceR = getJson
