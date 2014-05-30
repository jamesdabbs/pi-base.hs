module Model.Proof
( proofTraits
, proofTheorem
) where

import Import hiding ((==.))
import Database.Esqueleto

proofTraits :: ProofId -> Handler [Entity Trait]
proofTraits pid = runDB . select $
  from $ \(assumptions `InnerJoin` traits) -> do
  on (assumptions ^. AssumptionTraitId ==. traits ^. TraitId)
  where_ (assumptions ^. AssumptionProofId ==. val pid)
  return traits

proofTheorem :: Proof -> Handler (Entity Theorem)
proofTheorem proof = do
  let _id = proofTheoremId proof
  theorem <- runDB . get404 $ _id
  return $ Entity _id theorem
