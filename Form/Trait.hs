module Form.Trait
( createTraitForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3


layout :: BootstrapFormLayout
layout = BootstrapHorizontalForm {
  bflLabelOffset = ColMd 0,
  bflLabelSize   = ColMd 4,
  bflInputOffset = ColMd 0,
  bflInputSize   = ColMd 8
}

createTraitForm :: Html -> MForm Handler (FormResult Trait, Widget)
createTraitForm = renderBootstrap3 layout $ Trait
  <$> areq spaceField    (fs "Space")       Nothing
  <*> areq propertyField (fs "Property")    Nothing
  <*> areq valueField    (fs "Value")       Nothing
  <*> areq textField     (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> pure False
  <*  bootstrapSubmit ("Save" :: BootstrapSubmit Text)
