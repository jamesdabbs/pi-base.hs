module Handler.Theorems
( getTheoremsR
, postTheoremsR
, getTheoremR
, putTheoremR
, deleteTheoremR
, getTheoremRevisionsR
) where

import Import
import qualified Data.Set as S

import Explore (async, checkTheorem)
import Form (formulaField, runJsonForm)
import qualified Handler.Base as H
import Handler.Helpers (requireUser, requireAdmin, sendErrorMessage, invalid422)
import Logic (counterexamples)
import Models

createTheoremForm :: UTCTime -> FormInput Handler Theorem
createTheoremForm now = Theorem
  <$> ireq formulaField "antecedent"
  <*> ireq formulaField "consequent"
  <*> ireq textareaField "description"
  <*> pure []
  <*> pure now
  <*> pure now

updateTheoremForm :: (RenderMessage (HandlerSite m) FormMessage, Monad m) => Theorem -> UTCTime -> FormInput m Theorem
updateTheoremForm t now = Theorem
  <$> pure (theoremAntecedent t)
  <*> pure (theoremConsequent t)
  <*> ireq textareaField "description"
  <*> pure (theoremConverseIds t)
  <*> pure (theoremCreatedAt t)
  <*> pure now


-- FIXME!
theoremName :: Theorem -> Text
theoremName _ = ""

-- TODO: should this be the ToJSON instance of Theorems?
showTheorem :: Entity Theorem -> Value
showTheorem (Entity _id t) = object
  [ "id"         .= _id
  , "name"       .= theoremName t
  , "description".= theoremDescription t
  , "antecedent" .= theoremAntecedent t
  , "consequent" .= theoremConsequent t
  , "converse"   .= theoremConverseIds t
  ]

getTheoremsR :: Handler Value
getTheoremsR = H.index [Desc TheoremUpdatedAt] showTheorem

postTheoremsR :: Handler Value
postTheoremsR = do
  _ <- requireUser
  now <- lift getCurrentTime
  theorem <- runJsonForm $ createTheoremForm now
  cxs <- counterexamples . theoremImplication $ theorem
  if S.null cxs
    then do
      e@(Entity _id _) <- createWithRevision theorem
      theoremRecordProperties _id theorem
      async checkTheorem _id
      returnJson . showTheorem $ e
    else do
      sendErrorMessage invalid422 $ "Found counterexamples"

getTheoremR :: TheoremId -> Handler Value
getTheoremR = (runDB . get404) >=> returnJson

putTheoremR :: TheoremId -> Handler Value
putTheoremR _id = do
  _ <- requireAdmin
  theorem <- runDB $ get404 _id
  now <- liftIO getCurrentTime
  updated <- runJsonForm $ updateTheoremForm theorem now
  runDB $ replace _id updated
  updateWithRevision _id updated >>= returnJson . showTheorem

deleteTheoremR :: TheoremId -> Handler Value
deleteTheoremR _id = do
  _ <- requireAdmin
  theorem <- runDB $ get404 _id
  _ <- theoremDelete _id
  returnJson . showTheorem $ Entity _id theorem

getTheoremRevisionsR :: TheoremId -> Handler Value
getTheoremRevisionsR = H.revisions
