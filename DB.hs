module DB
( matches'
) where

import Import hiding ((==.), (!=.))

import Database.Esqueleto

import Logic.Types

matches' :: PropertyId -> TValueId -> MatchType -> Handler [SpaceId]
matches' p v Yes = runDB . fmap (map entityKey) . select $
  from $ \(s `InnerJoin` t) -> do
  on (s ^. SpaceId ==. t ^. TraitSpaceId)
  where_ (t ^. TraitPropertyId ==. (val p) &&. t ^. TraitValueId ==. (val v))
  return s
matches' p v No = runDB . fmap (map entityKey) . select $
  from $ \(s `InnerJoin` t) -> do
  on (s ^. SpaceId ==. t ^. TraitSpaceId)
  where_ (t ^. TraitPropertyId ==. (val p) &&. t ^. TraitValueId !=. (val v))
  return s
matches' p _ Unknown = runDB $ do
  ts <- select $
    from $ \t -> do
    where_ (t ^. TraitPropertyId ==. (val p))
    return t
  let knownIds = map (traitSpaceId . entityVal) $ ts
  ss <- select $
    from $ \s -> do
    where_ (s ^. SpaceId `notIn` (valList knownIds))
    return s
  return . map entityKey $ ss
