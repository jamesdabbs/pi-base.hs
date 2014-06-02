module Model.Space
( spaceTraitMap
, spaceManualTraits
, spaceRevisions
) where

import Import

import qualified Data.Set as S
import qualified Data.Map as M

spaceTraitMap :: SpaceId -> Set PropertyId -> Handler (TraitMap PropertyId)
spaceTraitMap sid ps = do
  ets <- runDB $ selectList [TraitSpaceId ==. sid, TraitPropertyId <-. (S.toList ps)] []
  return . M.fromList . map (\(Entity tid t) -> (traitPropertyId t, (tid, traitValueId t))) $ ets

spaceManualTraits :: SpaceId -> Handler [TraitId]
spaceManualTraits _id = do
  traits <- runDB $ selectList [TraitSpaceId ==. _id, TraitDeduced ==. False] []
  return . map entityKey $ traits

spaceRevisions :: SpaceId -> Handler [Entity Revision]
spaceRevisions _id = undefined
