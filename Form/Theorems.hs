module Form.Theorems
( createTheoremForm
, updateTheoremForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

createTheoremForm :: Html -> MForm Handler (FormResult Theorem, Widget)
createTheoremForm = renderBootstrap3 fLayout $ Theorem
  <$> areq textareaField (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> areq formulaField (fs "Antecedent") Nothing
  <*> areq formulaField (fs "Consequent") Nothing
  <*  save

updateTheoremForm :: Theorem -> Html -> MForm Handler (FormResult Theorem, Widget)
updateTheoremForm t = renderBootstrap3 fLayout $ Theorem
  <$> areq textareaField (fs "Description") (Just $ theoremDescription t)
  <*> pure (theoremCreatedAt t)
  <*> lift (liftIO getCurrentTime)
  <*> pure (theoremAntecedent t)
  <*> pure (theoremConsequent t)
  <*  save
