module Handler.Admin where

import Import

import qualified Data.Set as S

import DB (flushDeductions)
import Explore (checkTheorem)
import Logic (counterexamples)
import Models
import Handler.Helpers
import Handler.Partials (theoremName, traitName)

getAdminR :: Handler Html
getAdminR = render "Admin" $(widgetFile "admin/show")

-- TODO: add counts of added, with counterexamples
postExploreR :: Handler Html
postExploreR = do
  theorems <- runDB $ selectList [] []
  mapM_ (checkTheorem "Checking all theorems" . entityKey) theorems
  flash Success "Explored all theorems"
  redirect AdminR

postContradictionsR :: Handler Html
postContradictionsR = do
  theorems <- runDB $ selectList [] []
  counters <- mapM (counterexamples . theoremImplication . entityVal) $ theorems
  let pairs = filter (not . S.null . snd) $ zip theorems counters
  render "Contradictions" $(widgetFile "admin/check")

postResetR :: Handler Html
postResetR = do
#ifdef DEVELOPMENT
  flushDeductions
  flash Warning "Reset deduced theorems"
#else
  flash Danger "Can only reset in development mode"
#endif
  redirect AdminR

getUnprovenR :: Handler Html
getUnprovenR = do
  (traits, pager) <- paged 20 [TraitDeduced ==. False, TraitDescription ==. Textarea ""] [Asc TraitSpaceId]
  render "Unproven Assertions" $(widgetFile "admin/unproven")
