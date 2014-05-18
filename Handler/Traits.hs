module Handler.Traits where

import Import

import DB (traitConsequences, deleteTrait, proofTraits, proofTheorem, derivedTraits)
import Explore (checkTrait, checkSpace)
import Form.Traits (createTraitForm)
import Handler.Partials (traitName, linkedTraitName, theoremName)
import Handler.Resource (page)

-- FIXME
queueCheckTrait :: TraitId -> Handler ()
queueCheckTrait _id = do
  _ <- checkTrait "queueCheckTrait" _id
  return ()


getTraitsR :: Handler Html
getTraitsR = do
  (traits, pageWidget) <- page 10
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
  case traitDeduced trait of
    True -> do
      (Entity proofId proof) <- runDB . getBy404 $ UProofTrait _id
      assumedTraits  <- proofTraits  proofId
      assumedTheorem <- proofTheorem proof
      derived <- derivedTraits _id
      defaultLayout $(widgetFile "traits/show_deduced")
    False -> do
      consequences <- traitConsequences _id
      defaultLayout $(widgetFile "traits/show")

-- FIXME: only allow deleting manually added traits
getDeleteTraitR :: TraitId -> Handler Html
getDeleteTraitR _id = do
  trait <- runDB $ get404 _id
  consequences <- traitConsequences _id
  defaultLayout $(widgetFile "traits/delete")

postDeleteTraitR :: TraitId -> Handler Html
postDeleteTraitR _id = do
  trait <- runDB . get404 $ _id
  _ <- deleteTrait _id
  _ <- checkSpace "postDeleteTraitR" (traitSpaceId trait)
  setMessage "Deleted trait" -- TODO: show deleted / re-added counts
  redirect TraitsR

postCheckTraitR :: TraitId -> Handler Html
postCheckTraitR _id = do
  _ <- checkTrait "postCheckTraitR" _id -- TODO: better log name
  setMessage "Checked trait" -- TODO: show found count?
  redirect $ TraitR _id
