module Handler.Admin where

import Import

import Explore (checkTheorem)

getAdminR :: Handler Html
getAdminR = defaultLayout $(widgetFile "admin/show")

-- TODO: add counts of added, with counterexamples
postCheckLogicR :: Handler Html
postCheckLogicR = do
  theorems <- runDB $ selectList [] []
  mapM_ (checkTheorem "Checking all theorems" . entityKey) theorems
  redirect TheoremsR
