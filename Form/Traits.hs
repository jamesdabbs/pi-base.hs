module Form.Traits
( createTraitForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

createTraitForm :: Html -> MForm Handler (FormResult Trait, Widget)
createTraitForm = renderBootstrap3 fLayout $ Trait
  <$> areq spaceField    (fs "Space")       Nothing
  <*> areq propertyField (fs "Property")    Nothing
  <*> areq valueField    (fs "Value")       Nothing
  <*> areq textField     (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> pure False
  <*> pure Nothing
  <*  bootstrapSubmit ("Save" :: BootstrapSubmit Text)
