module Handler.Theorems where

import Import

import Control.Monad (void)
import DB (theoremConsequences, deleteTheorem)
import Explore (checkTheorem)
import Form.Theorems (createTheoremForm)
import Handler.Partials (traitName, theoremName)
import Handler.Resource (page)


-- FIXME
queueCheckTheorem :: TheoremId -> Handler ()
queueCheckTheorem = void . checkTheorem


getTheoremsR :: Handler Html
getTheoremsR = do
  theorems <- page 0 10
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

postCheckTheoremR :: TheoremId -> Handler Html
postCheckTheoremR _id = do
  theorem <- runDB $ get404 _id
  traits <- checkTheorem _id
  defaultLayout $(widgetFile "theorems/check")
