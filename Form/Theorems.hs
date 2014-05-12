module Form.Theorems
( createTheoremForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

import Data.Int (Int64)
import Logic.Types (Formula(..))

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
