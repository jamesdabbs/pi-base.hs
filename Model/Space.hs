module Model.Space
( spaceTraitMap
, spaceTraits
, spaceUnknownProperties
, SpaceCreateData (..)
, spaceCreate
, SpaceUpdateData (..)
, spaceUpdate
, spaceDelete
) where

import Import

import qualified Data.Set as S
import qualified Data.Map as M

import Model.Revision


data SpaceCreateData = SpaceCreateData
  { scdName :: Text
  , scdDescription :: Textarea
  , scdProofOfTopology :: Maybe Textarea
  }

data SpaceUpdateData = SpaceUpdateData
  { sudDescription :: Textarea
  , sudProofOfTopology :: Maybe Textarea
  }

spaceCreate :: SpaceCreateData -> Handler (Entity Space)
spaceCreate d = do
  now <- lift getCurrentTime
  let space = Space (scdName d) (scdDescription d) now now (scdProofOfTopology d)
  _id <- runDB $ insert space
  return $ Entity _id space

spaceUpdate :: Entity Space -> SpaceUpdateData -> Handler (Entity Space)
spaceUpdate (Entity _id s) d = do
  now <- lift getCurrentTime
  let updated = s { spaceDescription = sudDescription d, spaceProofOfTopology = sudProofOfTopology d, spaceUpdatedAt = now }
  runDB $ replace _id updated
  return $ Entity _id updated

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
