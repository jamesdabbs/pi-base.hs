module Handler.Admin where

import Import

import Control.Monad (filterM)
import qualified Data.Text as T
import qualified Data.Set as S

import Explore (checkTheorem)
import Logic (counterexamples, converse)
import Models
import Handler.Helpers
import Handler.Partials (theoremName)

#ifdef DEVELOPMENT
import DB (flushDeductions)
#endif

getAdminR :: Handler Html
getAdminR = render "Admin" $(widgetFile "admin/show")

postExploreR :: Handler Html
postExploreR = do
  before <- runDB $ count ([] :: [Filter Trait])
  theorems <- runDB $ selectList [] []
  mapM_ (checkTheorem "Checking all theorems" . entityKey) theorems
  after <- runDB $ count ([] :: [Filter Trait])
  flash Success $ "Explored all theorems. Found " <> (T.pack . show $ (after - before)) <> " new traits."
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

progressRow :: Entity Space -> Widget
progressRow (Entity _id s) = do
  known <- handlerToWidget . runDB $ count [TraitSpaceId ==. _id]
  unproven <- handlerToWidget . runDB $ count [TraitSpaceId ==. _id, TraitDeduced ==. False, TraitDescription ==. Textarea ""]
  [whamlet|<tr><td><a href=@{SpaceR _id}>#{spaceName s}</a></td><td>#{known}</td><td>#{unproven}</td>|]

theoremUnknownReversables :: Handler [Entity Theorem]
theoremUnknownReversables = do
  ts <- runDB $ selectList [] [Asc TheoremId]
  -- n+1
  filterM interesting $ ts
  where
    interesting (Entity _id t) = do
      if null . theoremConverseIds $ t
        then do
          cxs <- counterexamples . converse . theoremImplication $ t
          return . S.null $ cxs
        else return False

getTraitProgressR :: Handler Html
getTraitProgressR = do
  spaces <- runDB $ selectList [] [Asc SpaceName]
  render "Trait Progress" $(widgetFile "admin/trait_progress")

getTheoremProgressR :: Handler Html
getTheoremProgressR = do
  unprovenTheorems <- runDB $ selectList [TheoremDescription ==. Textarea ""] [Asc TheoremId]
  reversables <- theoremUnknownReversables
  render "Theorem Progress" $(widgetFile "admin/theorem_progress")
