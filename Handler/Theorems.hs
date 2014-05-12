module Handler.Theorems where

import Import

import Handler.Resource

import DB (theoremConsequences)


theoremName :: Theorem -> Widget
theoremName theorem = do
  toWidget [whamlet|<span> TODO: write theorem name widget|]

getTheoremsR :: Handler Value
getTheoremsR = do
  theorems <- page 0 10
  returnJson $ (theorems :: [Entity Theorem])

postTheoremsR :: Handler Html
postTheoremsR = undefined

getTheoremR :: TheoremId -> Handler Html
getTheoremR _id = do
  theorem <- runDB $ get404 _id
  defaultLayout $(widgetFile "theorems/show")

getDeleteTheoremR :: TheoremId -> Handler Html
getDeleteTheoremR _id = do
  theorem <- runDB $ get404 _id
  consequences <- theoremConsequences _id
  defaultLayout $(widgetFile "theorems/delete")

postDeleteTheoremR :: TheoremId -> Handler Html
postDeleteTheoremR _id = do
  consequences <- theoremConsequences _id
  undefined

postCheckTheoremR :: TheoremId -> Handler Html
postCheckTheoremR = undefined
