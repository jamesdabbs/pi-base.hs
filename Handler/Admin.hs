module Handler.Admin where

import Import

import qualified Data.Set as S

import DB (theoremImplication, flushDeductions)
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
  setMessage "Explored all theorems"
  redirect AdminR

postContradictionsR :: Handler Html
postContradictionsR = do
  theorems <- runDB $ selectList [] []
  counters <- mapM (counterexamples . theoremImplication . entityVal) $ theorems
  let pairs = filter (not . S.null . snd) $ zip theorems counters
  defaultLayout $(widgetFile "admin/check")

postResetR :: Handler Html
postResetR = do
#ifdef DEVELOPMENT
  flushDeductions
  setMessage "Reset deduced theorems"
#else
  setMessage "Can only reset in development mode"
#endif
  redirect AdminR
