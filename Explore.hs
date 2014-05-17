module Explore
( checkTrait
, checkSpace
, checkTheorem
) where

import Import

import qualified Data.Set as S

import DB (theoremImplication, traitMap, spaceManualTraits)
import Logic.Types
import Logic
import Util (unionN, encodeText)

checkTrait :: Text -> TraitId -> Handler Int
checkTrait desc _id = runQueue desc [_id]

checkSpace :: Text -> SpaceId -> Handler Int
checkSpace desc _id = do
  traitIds <- spaceManualTraits _id
  runQueue desc traitIds

runQueue :: Text -> [TraitId] -> Handler Int
runQueue name (t:ts) = do
    nts <- checkTraitStep t
    found <- runQueue name $ ts <> (map entityKey nts)
    return $ (length nts) + found
runQueue name _ = do
  $(logInfo) $ "Done with queue for " <> name
  return 0

checkTraitStep :: TraitId -> Handler [Entity Trait]
checkTraitStep _id = do
   t' <- runDB $ get _id
   case t' of
     Nothing -> do
       $(logError) $ "Could not find trait " <> (encodeText _id)
       return []
     Just t -> checkRelevantTheorems t

checkRelevantTheorems :: Trait -> Handler [Entity Trait]
checkRelevantTheorems trait = do
  pairs <- relevantTheorems trait
  let implications = map snd pairs
  tmap <- traitMap (traitSpaceId trait) (unionN . map implicationProperties $ implications)
  let proofs = concat . map (\(tid,i) -> apply' tid i tmap) $ pairs
  mapM (addProof . traitSpaceId $ trait) proofs

-- FIXME: update signature to match Trait, Space check
checkTheorem :: TheoremId -> Handler [Entity Trait]
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

checkCandidates :: TheoremId -> Implication PropertyId -> Handler [Entity Trait]
checkCandidates _id i = do
  ss <- candidates i
  tls <- mapM (apply _id i) . S.toList $ ss
  return . concat $ tls
