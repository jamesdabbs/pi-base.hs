module Model.Theorem
( theoremConsequences
, theoremDelete
, theoremImplication
) where

import Import hiding ((==.))
import qualified Import as I ((==.))

import Database.Esqueleto hiding (delete)

import DB (supportedTraits, deleteWithConsequences)
import Model.Revision

theoremConsequences :: TheoremId -> Handler [Entity Trait]
theoremConsequences _id = do
  error "This doesn't work, for two reasons:\
    1) We aren't including the proved traits in the return value\
    2) The proved traits are automatically deduced, and so don't track their support"
  proved <- runDB . select $
    from $ \(traits `InnerJoin` proofs) -> do
    on (traits ^. TraitId ==. proofs ^. ProofTraitId)
    where_ (proofs ^. ProofTheoremId ==. (val _id))
    return traits
  supportedTraits $ map entityKey proved

theoremDelete :: TheoremId -> Handler Int64
theoremDelete _id = do
  theorem <- runDB $ get404 _id
  logDeletion $ Entity _id theorem
  runDB $ deleteWhere [TheoremPropertyTheoremId I.==. _id]
  n <- deleteWithConsequences theoremConsequences _id
  runDB $ delete _id
  return n

theoremImplication :: Theorem -> Implication PropertyId
theoremImplication t = (Key . PersistInt64) <$> Implication (theoremAntecedent t) (theoremConsequent t)
