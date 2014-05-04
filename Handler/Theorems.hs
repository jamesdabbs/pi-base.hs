module Handler.Theorems where

import Import

import Handler.Resource

getTheoremsR :: Handler Value
getTheoremsR = do
  theorems <- page 0 10
  returnJson $ (theorems :: [Entity Theorem])

getTheoremR :: TheoremId -> Handler Value
getTheoremR = getJson
