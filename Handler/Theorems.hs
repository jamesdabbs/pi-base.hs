module Handler.Theorems where

import Import

import DB (theoremConsequences, deleteTheorem)
import Explore (checkTheorem)
import Form.Theorems (createTheoremForm)
import Handler.Partials (linkedTheoremName, theoremName, traitName)
import Handler.Helpers


-- FIXME
queueCheckTheorem :: TheoremId -> Handler ()
queueCheckTheorem _id = do
  _ <- checkTheorem "queueCheckTheorem" _id
  return ()


getTheoremsR :: Handler Html
getTheoremsR = do
  (theorems, total, pageWidget) <- page 10
  defaultLayout $(widgetFile "theorems/index")

getCreateTheoremR :: Handler Html
getCreateTheoremR = do
  (widget, enctype) <- generateFormPost createTheoremForm
  defaultLayout $(widgetFile "theorems/new")

postCreateTheoremR :: Handler Html
postCreateTheoremR = do
  ((result, widget), enctype) <- runFormPost createTheoremForm
  case result of
    FormSuccess theorem -> do
      _id <- runDB $ insert theorem
      queueCheckTheorem _id
      setMessage "Created theorem"
      redirect $ TheoremR _id
    _ -> defaultLayout $(widgetFile "theorems/new")

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
  _ <- deleteTheorem _id
  setMessage "Deleted theorem"
  redirect TheoremsR
