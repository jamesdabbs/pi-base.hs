module Explore
( checkTrait
, checkTheorem
, runQueue
) where

import Import

import qualified Data.Set as S

import DB (theoremImplication, traitMap)
import Logic.Types
import Logic
import Util (unionN, encodeText)

runQueue :: Text -> [TraitId] -> Handler ()
runQueue name (t:ts) = do
    nts <- checkTrait t
    runQueue name $ ts <> nts
runQueue name _ = $(logInfo) $ "Done with queue for " <> name

checkTrait :: TraitId -> Handler [TraitId]
checkTrait _id = do
   t' <- runDB $ get _id
   case t' of
     Nothing -> do
       $(logError) $ "Could not find trait " <> (encodeText _id)
       return []
     Just t -> checkRelevantTheorems t

checkRelevantTheorems :: Trait -> Handler [TraitId]
checkRelevantTheorems trait = do
  pairs <- relevantTheorems trait
  let implications = map snd pairs
  tmap <- traitMap (traitSpaceId trait) (unionN . map implicationProperties $ implications)
  let proofs = concat . map (\(tid,i) -> apply' tid i tmap) $ pairs
  mapM (addProof . traitSpaceId $ trait) proofs

checkTheorem :: TheoremId -> Handler [TraitId]
checkTheorem _id = do
  t' <- runDB $ get _id
  case t' of
    Nothing -> do
      $(logError) $ "Could not find theorem " <> (encodeText _id)
      return []
    Just t -> do
      direct <- checkCandidates _id (theoremImplication t)
      contra <- checkCandidates _id (contrapositive . theoremImplication $ t)
      return $ direct <> contra

checkCandidates :: TheoremId -> Implication PropertyId -> Handler [TraitId]
checkCandidates _id i = do
  ss <- candidates i
  tls <- mapM (apply _id i) . S.toList $ ss
  return . concat $ tls
