module Form.Space
( createSpaceForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

createSpaceForm :: Html -> MForm Handler (FormResult Space, Widget)
createSpaceForm = renderBootstrap3 fLayout $ Space
  <$> areq textField (fs "Name")        Nothing
  <*> areq textField (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> aopt textField (fs "Proof of Topology") Nothing
  <*  bootstrapSubmit ("Save" :: BootstrapSubmit Text)
