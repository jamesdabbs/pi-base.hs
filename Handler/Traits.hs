module Handler.Traits where

import Import

import DB (traitConsequences, deleteTraitConsequences)
import Form.Trait (createTraitForm)
import Handler.Resource (page)


traitName :: Trait -> Widget
traitName trait = do
  s <- wget . traitSpaceId $ trait
  p <- wget . traitPropertyId $ trait
  v <- wget . traitValueId $ trait
  toWidget [whamlet|<span>#{spaceName s}: #{propertyName p}=#{tValueName v}|]
  where
    wget = handlerToWidget . runDB . get404

queueCheckTrait :: TraitId -> Handler ()
queueCheckTrait = undefined

checkTrait :: TraitId -> Handler [Entity Trait]
checkTrait = undefined


getTraitsR :: Handler Html
getTraitsR = do
  traits <- page 0 10
  defaultLayout $(widgetFile "traits/index")

getCreateTraitR :: Handler Html
getCreateTraitR = do
  (widget, enctype) <- generateFormPost createTraitForm
  defaultLayout $(widgetFile "traits/new")

postCreateTraitR :: Handler Html
postCreateTraitR = do
  ((result, widget), enctype) <- runFormPost createTraitForm
  case result of
    FormSuccess trait -> do
      existing <- runDB . getBy $ TraitSP (traitSpaceId trait) (traitPropertyId trait)
      case existing of
        Just (Entity _id _) -> do
          setMessage "Trait already exists"
          redirect $ TraitR _id
        Nothing -> do
          _id <- runDB $ insert trait
          queueCheckTrait _id
          setMessage "Created trait"
          redirect $ TraitR _id
    _ -> defaultLayout $(widgetFile "traits/new")

getTraitR :: TraitId -> Handler Html
getTraitR _id = do
  trait <- runDB $ get404 _id
  defaultLayout $(widgetFile "traits/show")

getDeleteTraitR :: TraitId -> Handler Html
getDeleteTraitR _id = do
  trait <- runDB $ get404 _id
  consequences <- traitConsequences _id
  defaultLayout $(widgetFile "traits/delete")

postDeleteTraitR :: TraitId -> Handler Html
postDeleteTraitR _id = do
  _ <- deleteTraitConsequences _id
  setMessage "Deleted trait"
  redirect TraitsR

postCheckTraitR :: TraitId -> Handler Html
postCheckTraitR _id = do
  trait <- runDB $ get404 _id
  traits <- checkTrait _id
  defaultLayout $(widgetFile "traits/check")
