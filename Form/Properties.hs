module Form.Properties
( createPropertyForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

boolean = undefined

createPropertyForm :: Html -> MForm Handler (FormResult Property, Widget)
createPropertyForm = renderBootstrap3 fLayout $ Property
  <$> areq textField (fs "Name")        Nothing
  <*> areq textField (fs "Description") Nothing
  <*> lift (boolean)
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*  bootstrapSubmit ("Save" :: BootstrapSubmit Text)
