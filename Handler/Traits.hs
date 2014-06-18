module Handler.Traits where

import Import

import qualified Data.Text as T

import DB (derivedTraits)
import Explore (async, checkTrait, checkSpace)
import Form.Traits
import Handler.Partials (revisionList)
import Handler.Helpers
import Models
import Presenter.Trait
import Presenter.Theorem (theoremName)


-- FIXME
traitTitle :: Trait -> Text
traitTitle _ = "Trait"


getTraitsR :: Handler Html
getTraitsR = do
  total <- runDB $ count ([] :: [Filter Trait])
  (traits, pager) <- paged 25 [] [Desc TraitUpdatedAt]
  (spaces, properties) <- traitPrefetch traits
  render "Traits" $(widgetFile "traits/index")

getCreateTraitR :: SpaceId -> Handler Html
getCreateTraitR sid = do
  space <- runDB $ get404 sid
  (widget, enctype) <- generateFormPost $ createTraitForm sid
  render "New Trait" $(widgetFile "traits/new")

postCreateTraitR :: SpaceId -> Handler Html
postCreateTraitR sid = do
  space <- runDB $ get404 sid
  ((result, widget), enctype) <- runFormPost $ createTraitForm sid
  case result of
    FormSuccess trait -> do
      existing <- runDB . getBy $ TraitSP (traitSpaceId trait) (traitPropertyId trait)
      case existing of
        Just (Entity _id _) -> do
          flash Danger "Trait already exists"
          redirect $ TraitR _id
        Nothing -> do
          _id <- runDB $ insert trait
          _ <- revisionCreate $ Entity _id trait
          async checkTrait _id
          flash Success "Created trait"
          redirect $ TraitR _id
    _ -> render "New Trait" $(widgetFile "traits/new")

getEditTraitR :: TraitId -> Handler Html
getEditTraitR _id = do
  trait <- runDB $ get404 _id
  (widget, enctype) <- generateFormPost $ updateTraitForm trait
  (spaces, properties) <- traitPrefetch [Entity _id trait]
  render ("Edit " <> traitTitle trait) $(widgetFile "traits/edit")

postTraitR :: TraitId -> Handler Html
postTraitR _id = do
  trait <- runDB $ get404 _id
  ((result, widget), enctype) <- runFormPost $ updateTraitForm trait
  case result of
    FormSuccess updated -> do
      runDB $ replace _id updated
      _ <- revisionCreate $ Entity _id updated
      flash Success "Updated trait"
      redirect $ TraitR _id
    _ -> do
      (spaces, properties) <- traitPrefetch [Entity _id trait]
      render ("Edit " <> traitTitle trait) $(widgetFile "traits/edit")

getTraitR :: TraitId -> Handler Html
getTraitR _id = do
  trait <- runDB $ get404 _id
  case traitDeduced trait of
    True -> do
      (Entity proofId proof) <- runDB . getBy404 $ UProofTrait _id
      assumedTraits  <- proofTraits  proofId
      assumedTheorem <- proofTheorem proof
      derived        <- derivedTraits _id
      supports       <- traitSupport _id
      (spaces, properties) <- traitPrefetch $ [Entity _id trait] ++ derived ++ supports ++ assumedTraits
      theoremProperties    <- theoremPrefetch [entityVal assumedTheorem]
      render (traitTitle trait) $(widgetFile "traits/show_deduced")
    False -> do
      consequences <- traitConsequences _id
      (spaces, properties) <- traitPrefetch $ [Entity _id trait] ++ consequences
      render (traitTitle trait) $(widgetFile "traits/show")

getDeleteTraitR :: TraitId -> Handler Html
getDeleteTraitR _id = do
  trait <- runDB $ get404 _id
  consequences <- traitConsequences _id
  (spaces, properties) <- traitPrefetch $ [Entity _id trait] ++ consequences
  render ("Delete " <> traitTitle trait) $(widgetFile "traits/delete")

postDeleteTraitR :: TraitId -> Handler Html
postDeleteTraitR _id = do
  trait <- runDB $ get404 _id
  if traitDeduced trait
    then invalidArgs ["Please delete the root assumption for this trait"]
    else do
      _ <- traitDelete _id
      async checkSpace $ traitSpaceId trait
      flash Warning "Deleted trait"
      redirect . SpaceR . traitSpaceId $ trait

getTraitRevisionsR :: TraitId -> Handler Html
getTraitRevisionsR _id = do
  trait <- runDB $ get404 _id
  render (traitTitle trait <> " Revisions") $(widgetFile "traits/revisions")
