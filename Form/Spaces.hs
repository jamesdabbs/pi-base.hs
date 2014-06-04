module Form.Spaces
( createSpaceForm
, updateSpaceForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

createSpaceForm :: Html -> MForm Handler (FormResult Space, Widget)
createSpaceForm = renderBootstrap3 fLayout $ Space
  <$> areq textField (fs "Name") Nothing
  <*> areq textareaField (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> aopt textareaField (fs "Proof of Topology") Nothing
  <*  save

updateSpaceForm :: Space -> Html -> MForm Handler (FormResult Space, Widget)
updateSpaceForm s = renderBootstrap3 fLayout $ Space
  <$> pure (spaceName s)
  <*> areq textareaField (fs "Description") (Just $ spaceDescription s)
  <*> pure (spaceCreatedAt s)
  <*> lift (liftIO getCurrentTime)
  <*> aopt textareaField (fs "Proof of Topology") (Just $ spaceProofOfTopology s)
  <*  save
