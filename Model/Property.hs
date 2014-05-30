module Model.Property
( propertyTheorems
) where

import Import hiding ((==.))
import Database.Esqueleto

propertyTheorems :: PropertyId -> Handler [Entity Theorem]
propertyTheorems pid = runDB . select $
  from $ \(t `InnerJoin` pt) -> do
  on (t ^. TheoremId ==. pt ^. TheoremPropertyTheoremId)
  where_ (pt ^. TheoremPropertyPropertyId ==. val pid)
  return t
