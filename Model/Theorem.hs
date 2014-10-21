module Model.Theorem
( theoremConsequences
, theoremDelete
, theoremImplication
, theoremConverses
, theoremRecordProperties
, theoremPrefetch
) where

import Import hiding ((==.))
import qualified Import as I ((==.))

import qualified Data.Set as S

import Database.Esqueleto hiding (delete)
import qualified Database.Persist.Sql as SQL

import DB (deleteWithConsequences, prefetch, Prefetch)
import Model.Revision
import Util (unionN)

theoremConsequences :: TheoremId -> Handler [Entity Trait]
theoremConsequences _id = runDB . select $
  from $ \(traits `InnerJoin` struts) -> do
  on (traits ^. TraitId ==. struts ^. StrutTraitId)
  where_ (struts ^. StrutTheoremId ==. (val _id))
  return traits

theoremDelete :: TheoremId -> Handler Int64
theoremDelete _id = do
  theorem <- runDB $ get404 _id
  logDeletion $ Entity _id theorem
  runDB $ deleteWhere [TheoremPropertyTheoremId I.==. _id]
  n <- deleteWithConsequences theoremConsequences _id
  runDB $ delete _id
  return n

theoremImplication :: Theorem -> Implication PropertyId
theoremImplication t = (PropertyKey . SQL.SqlBackendKey) <$> Implication (theoremAntecedent t) (theoremConsequent t)

theoremConverses :: Theorem -> Handler [Entity Theorem]
theoremConverses t = runDB $ selectList [TheoremId <-. theoremConverseIds t] []

theoremRecordProperties :: TheoremId -> Theorem -> Handler ()
theoremRecordProperties _id t = mapM_ recordProperty properties
  where
    recordProperty p = runDB . insert $ TheoremProperty _id p
    properties = S.toList . implicationProperties . theoremImplication $ t

theoremPrefetch :: [Theorem] -> Handler (Prefetch Property)
theoremPrefetch ts = prefetch [PropertyId <-. S.toList pids]
  where
    pids = unionN . map (implicationProperties . theoremImplication) $ ts
