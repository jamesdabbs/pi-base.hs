module Handler.Theorems where

import Import
import qualified Data.Set as S
import Prelude (head)

import Explore (checkTheorem)
import Form.Theorems
import Handler.Partials (linkedTheoremName, theoremName, revisionList, linkedTraitList)
import Handler.Helpers
import Logic (counterexamples, converse)
import Models


-- FIXME
queueCheckTheorem :: TheoremId -> Handler ()
queueCheckTheorem _id = do
  _ <- checkTheorem "queueCheckTheorem" _id
  return ()

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
  (theorems, pager) <- paged 10 [] [Desc TheoremUpdatedAt]
  total <- runDB $ count ([] :: [Filter Theorem])
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
          queueCheckTheorem _id
          flash Success "Created theorem"
          redirect $ TheoremR _id
        else do
          -- TODO: better way to do this include?
          let baseW = $(widgetFile "theorems/new")
          let cx = head . S.toList $ cxs
          render "New Theorem" $(widgetFile "theorems/counterexamples")
    _ -> render "New Theorem" $(widgetFile "theorems/new")

getEditTheoremR :: TheoremId -> Handler Html
getEditTheoremR _id = do
  theorem <- runDB $ get404 _id
  (widget, enctype) <- generateFormPost $ updateTheoremForm theorem
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
    _ -> render ("Edit " <> theoremTitle theorem) $(widgetFile "theorems/edit")

getTheoremR :: TheoremId -> Handler Html
getTheoremR _id = do
  theorem <- runDB $ get404 _id
  converses <- theoremConverses theorem
  render (theoremTitle theorem) $(widgetFile "theorems/show")

getDeleteTheoremR :: TheoremId -> Handler Html
getDeleteTheoremR _id = do
  theorem <- runDB $ get404 _id
  consequences <- theoremConsequences _id
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
