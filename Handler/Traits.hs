module Handler.Traits where

import Import

import DB (derivedTraits)
import Explore (checkTrait, checkSpace)
import Form.Traits (createTraitForm)
import Handler.Partials (traitName, linkedTraitName, theoremName)
import Handler.Helpers
import Models

-- FIXME
queueCheckTrait :: TraitId -> Handler ()
queueCheckTrait _id = do
  _ <- checkTrait "queueCheckTrait" _id
  return ()


getTraitsR :: Handler Html
getTraitsR = do
  (traits, pageWidget) <- page 10
  total <- runDB $ count ([] :: [Filter Trait])
  render "Traits" $(widgetFile "traits/index")

getCreateTraitR :: Handler Html
getCreateTraitR = do
  (widget, enctype) <- generateFormPost createTraitForm
  render "New Trait" $(widgetFile "traits/new")

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
    _ -> render "New Trait" $(widgetFile "traits/new")

-- FIXME: need suitable string name here and for delete
getTraitR :: TraitId -> Handler Html
getTraitR _id = do
  trait <- runDB $ get404 _id
  case traitDeduced trait of
    True -> do
      (Entity proofId proof) <- runDB . getBy404 $ UProofTrait _id
      assumedTraits  <- proofTraits  proofId
      assumedTheorem <- proofTheorem proof
      derived        <- derivedTraits _id
      defaultLayout $(widgetFile "traits/show_deduced")
    False -> do
      consequences <- traitConsequences _id
      defaultLayout $(widgetFile "traits/show")

getDeleteTraitR :: TraitId -> Handler Html
getDeleteTraitR _id = do
  trait <- runDB $ get404 _id
  consequences <- traitConsequences _id
  defaultLayout $(widgetFile "traits/delete")

postDeleteTraitR :: TraitId -> Handler Html
postDeleteTraitR _id = do
  trait <- runDB . get404 $ _id
  if traitDeduced trait
    then invalidArgs ["Please delete the root assumption for this trait"]
    else do
      _ <- traitDelete _id
      _ <- checkSpace "postDeleteTraitR" (traitSpaceId trait)
      setMessage "Deleted trait" -- TODO: show deleted / re-added counts
      redirect TraitsR
