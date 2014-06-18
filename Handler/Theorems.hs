module Handler.Theorems where

import Import
import qualified Data.Set as S
import Prelude (head)

import Explore (async, checkTheorem)
import Form.Theorems
import Handler.Helpers
import Handler.Partials (revisionList)
import Logic (counterexamples, converse)
import Models
import Presenter.Trait (traitListLinked)
import Presenter.Theorem


-- FIXME
theoremTitle :: Theorem -> Text
theoremTitle _ = "Theorem"

searchHelp :: Widget
searchHelp = do
  let s = SpaceR . Key . PersistInt64
  $(widgetFile "search/help")

theoremConverseCounterexamples :: Theorem -> Widget
theoremConverseCounterexamples theorem = do
  cxids <- handlerToWidget . counterexamples . converse $ theoremImplication theorem
  let total = S.size cxids
  cxs <- handlerToWidget . runDB $ selectList [SpaceId <-. S.toList cxids] [LimitTo 5]
  $(widgetFile "theorems/_converse_counterexamples")

getTheoremsR :: Handler Html
getTheoremsR = do
  total <- runDB $ count ([] :: [Filter Theorem])
  (theorems, pager) <- paged 10 [] [Desc TheoremUpdatedAt]
  properties <- theoremPrefetch $ map entityVal theorems
  render "Theorems" $(widgetFile "theorems/index")

getCreateTheoremR :: Handler Html
getCreateTheoremR = do
  (widget, enctype) <- generateFormPost createTheoremForm
  render "New Theorem" $(widgetFile "theorems/new")

postCreateTheoremR :: Handler Html
postCreateTheoremR = do
  ((result, widget), enctype) <- runFormPost createTheoremForm
  case result of
    FormSuccess theorem -> do
      cxs <- counterexamples . theoremImplication $ theorem
      if S.null cxs
        then do
          _id <- runDB $ insert theorem
          _ <- revisionCreate $ Entity _id theorem
          theoremRecordProperties _id theorem
          async checkTheorem _id
          flash Success "Created theorem"
          redirect $ TheoremR _id
        else do
          -- TODO: better way to do this include?
          --   This should probably redirect to a conjecturer
          let baseW = $(widgetFile "theorems/new")
          let cx = head . S.toList $ cxs
          properties <- theoremPrefetch [theorem]
          render "New Theorem" $(widgetFile "theorems/counterexamples")
    _ -> render "New Theorem" $(widgetFile "theorems/new")

getEditTheoremR :: TheoremId -> Handler Html
getEditTheoremR _id = do
  theorem <- runDB $ get404 _id
  (widget, enctype) <- generateFormPost $ updateTheoremForm theorem
  properties <- theoremPrefetch [theorem]
  render ("Edit " <> theoremTitle theorem) $(widgetFile "theorems/edit")

postTheoremR :: TheoremId -> Handler Html
postTheoremR _id = do
  theorem <- runDB $ get404 _id
  ((result, widget), enctype) <- runFormPost $ updateTheoremForm theorem
  case result of
    FormSuccess updated -> do
      runDB $ replace _id updated
      _ <- revisionCreate $ Entity _id updated
      flash Success "Updated theorem"
      redirect $ TheoremR _id
    _ -> do
      properties <- theoremPrefetch [theorem]
      render ("Edit " <> theoremTitle theorem) $(widgetFile "theorems/edit")

getTheoremR :: TheoremId -> Handler Html
getTheoremR _id = do
  theorem <- runDB $ get404 _id
  converses <- theoremConverses theorem
  properties <- theoremPrefetch $ [theorem] ++ map entityVal converses
  render (theoremTitle theorem) $(widgetFile "theorems/show")

getDeleteTheoremR :: TheoremId -> Handler Html
getDeleteTheoremR _id = do
  theorem <- runDB $ get404 _id
  tprops <- theoremPrefetch [theorem]
  consequences <- theoremConsequences _id
  (spaces, properties) <- traitPrefetch consequences
  render ("Delete " <> theoremTitle theorem) $(widgetFile "theorems/delete")

postDeleteTheoremR :: TheoremId -> Handler Html
postDeleteTheoremR _id = do
  _ <- theoremDelete _id
  flash Warning "Deleted theorem"
  redirect TheoremsR

getTheoremRevisionsR :: TheoremId -> Handler Html
getTheoremRevisionsR _id = do
  theorem <- runDB . get404 $ _id
  render (theoremTitle theorem <> " Revisions") $(widgetFile "theorems/revisions")
