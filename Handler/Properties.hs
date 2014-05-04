module Handler.Properties where

import Import

import Handler.Resource

getPropertiesR :: Handler Value
getPropertiesR = do
  props <- page 0 10
  returnJson $ (props :: [Entity Property])

getPropertyR :: PropertyId -> Handler Value
getPropertyR = getJson
