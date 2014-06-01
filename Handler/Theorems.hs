module Handler.Theorems where

import Import

import Explore (checkTheorem)
import Form.Theorems (createTheoremForm)
import Handler.Partials (linkedTheoremName, theoremName, traitName)
import Handler.Helpers
import Models


-- FIXME
queueCheckTheorem :: TheoremId -> Handler ()
queueCheckTheorem _id = do
  _ <- checkTheorem "queueCheckTheorem" _id
  return ()


getTheoremsR :: Handler Html
getTheoremsR = do
  (theorems, pageWidget) <- page 10
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
      _id <- runDB $ insert theorem
      queueCheckTheorem _id
      setMessage "Created theorem"
      redirect $ TheoremR _id
    _ -> render "New Theorem" $(widgetFile "theorems/new")

-- FIXME: need a suitable string renderer for the title here and in delete
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
  _ <- theoremDelete _id
  setMessage "Deleted theorem"
  redirect TheoremsR
