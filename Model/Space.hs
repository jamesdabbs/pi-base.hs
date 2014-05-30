module Model.Space
( spaceTraitMap
, spaceManualTraits
) where

import Import hiding ((==.))
import Database.Esqueleto

import qualified Data.Set as S
import qualified Data.Map as M

spaceTraitMap :: SpaceId -> Set PropertyId -> Handler (TraitMap PropertyId)
spaceTraitMap sid ps = runDB $ do
  ets <- select $
    from $ \t -> do
    where_ $ (t ^. TraitSpaceId ==. val sid &&. t ^. TraitPropertyId `in_` (valList . S.toList $ ps))
    return t
  return . M.fromList . map (\(Entity tid t) -> (traitPropertyId t, (tid, traitValueId t))) $ ets

spaceManualTraits :: SpaceId -> Handler [TraitId]
spaceManualTraits _id = do
  traits <- runDB . select $
    from $ \(traits) -> do
    where_ (traits ^. TraitSpaceId ==. (val _id) &&. traits ^. TraitDeduced ==. (val False))
    return traits
  return . map entityKey $ traits
