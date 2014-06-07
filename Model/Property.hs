module Model.Property
( propertyTheorems
, propertyDelete
, propertyNames
) where

import Import hiding ((==.))
import qualified Import as I ((==.))

import Database.Esqueleto hiding (delete)

import Model.Revision

propertyTheorems :: PropertyId -> Handler [Entity Theorem]
propertyTheorems pid = runDB . select $
  from $ \(t `InnerJoin` pt) -> do
  on (t ^. TheoremId ==. pt ^. TheoremPropertyTheoremId)
  where_ (pt ^. TheoremPropertyPropertyId ==. val pid)
  return t

propertyDelete :: PropertyId -> Handler ()
propertyDelete _id = do
  property <- runDB $ get404 _id
  logDeletion $ Entity _id property
  runDB $ deleteWhere [TraitPropertyId I.==. _id]
  runDB $ delete _id

propertyNames :: Property -> [Text]
propertyNames p = propertyName p : propertyAliases p
