module Handler.Admin where

import Import

import qualified Data.Set as S

import DB (theoremImplication)
import Explore (checkTheorem)
import Logic (counterexamples)
import Handler.Partials (theoremName)

getAdminR :: Handler Html
getAdminR = defaultLayout $(widgetFile "admin/show")

-- TODO: add counts of added, with counterexamples
postExploreR :: Handler Html
postExploreR = do
  theorems <- runDB $ selectList [] []
  mapM_ (checkTheorem "Checking all theorems" . entityKey) theorems
  redirect TheoremsR

postCounterexamplesR :: Handler Html
postCounterexamplesR = do
  theorems <- runDB $ selectList [] []
  counters <- mapM (counterexamples . theoremImplication . entityVal) $ theorems
  let pairs = filter (not . S.null . snd) $ zip theorems counters
  defaultLayout $(widgetFile "admin/check")
