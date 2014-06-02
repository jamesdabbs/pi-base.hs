module Form.Theorems
( createTheoremForm
, updateTheoremForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

-- FIXME: add validation / coercion
formulaField :: Field Handler (Formula Int64)
formulaField = undefined

createTheoremForm :: Html -> MForm Handler (FormResult Theorem, Widget)
createTheoremForm = renderBootstrap3 fLayout $ Theorem
  <$> areq textField (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> areq formulaField (fs "Antecedent") Nothing
  <*> areq formulaField (fs "Consequent") Nothing
  <*  bootstrapSubmit ("Save" :: BootstrapSubmit Text)

updateTheoremForm :: Theorem -> Html -> MForm Handler (FormResult Theorem, Widget)
updateTheoremForm t = renderBootstrap3 fLayout $ Theorem
  <$> areq textField (fs "Description") (Just $ theoremDescription t)
  <*> pure (theoremCreatedAt t)
  <*> lift (liftIO getCurrentTime)
  <*> pure (theoremAntecedent t)
  <*> pure (theoremConsequent t)
  <* bootstrapSubmit ("Save" :: BootstrapSubmit Text)
