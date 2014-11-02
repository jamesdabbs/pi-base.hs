module Handler.Traits where

import Import

import Control.Monad ((>=>))
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import DB (derivedTraits)
import Explore (async, checkTrait, checkSpace)
import Form (runJsonForm)
import Handler.Partials (revisionList)
import Handler.Helpers
import Models

createTraitForm = undefined
updateTraitForm = undefined


getTraitsR :: Handler Value
getTraitsR = paged' [] [Desc TraitUpdatedAt] >>= returnJson

postTraitsR :: Handler Value
postTraitsR = do
  _ <- requireUser
  now <- lift getCurrentTime
  trait <- runJsonForm $ createTraitForm now
  existing <- runDB . getBy $ TraitSP (traitSpaceId trait) (traitPropertyId trait)
  case existing of
    Just trait -> do
      error "Should render 422"
    Nothing -> do
      _id <- runDB $ insert trait
      -- FIXME: _ <- revisionCreate $ Entity _id trait
      async checkTrait _id
      returnJson $ Entity _id trait

-- TODO: show deduced, supports, etc
getTraitR :: TraitId -> Handler Value
getTraitR = (runDB . get404) >=> returnJson

putTraitR :: TraitId -> Handler Value
putTraitR _id = do
  _ <- requireAdmin
  trait <- runDB $ get404 _id
  now <- liftIO getCurrentTime
  updated <- runJsonForm $ updateTraitForm trait now
  runDB $ replace _id updated
  returnJson $ Entity _id updated

deleteTraitR :: TraitId -> Handler Value
deleteTraitR _id = do
  _ <- requireAdmin
  trait <- runDB $ get404 _id
  _ <- traitDelete _id
  returnJson $ Entity _id trait
