module Model.Theorem
( theoremConsequences
, theoremDelete
, theoremImplication
) where

import Import hiding ((==.))

import Database.Esqueleto hiding (delete)

import DB (supportedTraits, deleteConsequences)

theoremConsequences :: TheoremId -> Handler [Entity Trait]
theoremConsequences _id = do
  proved <- runDB . select $
    from $ \(traits `InnerJoin` proofs) -> do
    on (traits ^. TraitId ==. proofs ^. ProofTraitId)
    where_ (proofs ^. ProofTheoremId ==. (val _id))
    return traits
  supportedTraits $ map entityKey proved

theoremDelete :: TheoremId -> Handler Int64
theoremDelete _id = do
  consequences <- theoremConsequences _id
  runDB $ delete _id
  deleteConsequences consequences

theoremImplication :: Theorem -> Implication PropertyId
theoremImplication t = (Key . PersistInt64) <$> Implication (theoremAntecedent t) (theoremConsequent t)
