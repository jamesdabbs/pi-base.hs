module Form.Traits
( createTraitForm
, updateTraitForm
) where

import Import
import Form

import Control.Applicative ((<*))
import Data.Time (getCurrentTime)
import Yesod.Form.Bootstrap3

createTraitForm :: SpaceId -> Html -> MForm Handler (FormResult Trait, Widget)
createTraitForm sid = renderBootstrap3 fLayout $ Trait
  <$> pure sid
  <*> areq (propertyField sid) (fs "Property") Nothing
  <*> areq valueField    (fs "Value")       Nothing
  <*> areq textareaField (fs "Description") Nothing
  <*> lift (liftIO getCurrentTime)
  <*> lift (liftIO getCurrentTime)
  <*> pure False
  <*  save

updateTraitForm :: Trait -> Html -> MForm Handler (FormResult Trait, Widget)
updateTraitForm t = renderBootstrap3 fLayout $ Trait
  <$> pure (traitSpaceId t)
  <*> pure (traitPropertyId t)
  <*> pure (traitValueId t)
  <*> areq textareaField (fs "Description") (Just $ traitDescription t)
  <*> pure (traitCreatedAt t)
  <*> lift (liftIO getCurrentTime)
  <*> pure (traitDeduced t)
  <*  save
