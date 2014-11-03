module Model.Space
( spaceTraitMap
, spaceTraits
, spaceUnknownProperties
, spaceDelete
) where

import Import

import qualified Data.Set as S
import qualified Data.Map as M

import Model.Revision


spaceTraitMap :: SpaceId -> Set PropertyId -> Handler (TraitMap PropertyId)
spaceTraitMap sid ps = do
  ets <- runDB $ selectList [TraitSpaceId ==. sid, TraitPropertyId <-. (S.toList ps)] []
  return . M.fromList . map (\(Entity tid t) -> (traitPropertyId t, (tid, traitValueId t))) $ ets

spaceTraits :: SpaceId -> Handler [TraitId]
spaceTraits _id = runDB $ selectKeysList [TraitSpaceId ==. _id] []

spaceUnknownProperties :: SpaceId -> Handler [Entity Property]
spaceUnknownProperties _id = do
  knownTraits <- runDB $ selectList [TraitSpaceId ==. _id] []
  let knownProperties = map (traitPropertyId . entityVal) knownTraits
  runDB $ selectList [PropertyId /<-. knownProperties] [Asc PropertyName]

spaceDelete :: SpaceId -> Handler ()
spaceDelete _id = do
  space <- runDB $ get404 _id
  logDeletion $ Entity _id space
  runDB $ deleteWhere [TraitSpaceId ==. _id]
  runDB $ delete _id
