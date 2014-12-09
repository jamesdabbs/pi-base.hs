module Explore
( async
, checkTrait
, checkSpace
, checkTheorem
) where

import Import

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Data.Set as S

import Logic
import Models
import Util (unionN, encodeText)

async :: ToJSON a => (Text -> a -> Handler b) -> a -> Handler ()
async checker val = do
  runner <- handlerToIO
  void . liftIO . forkIO $ void . runner $ checker label val
  where label = "Async queue for " <> encodeText val

checkTrait :: Text -> TraitId -> Handler Int
checkTrait desc _id = runQueue desc [_id]

checkSpace :: Text -> SpaceId -> Handler Int
checkSpace desc _id = do
  traitIds <- spaceTraits _id
  runQueue desc traitIds

checkTheorem :: Text -> TheoremId -> Handler Int
checkTheorem desc _id = do
  tids <- checkTheoremStep _id
  runQueue desc tids

runQueue :: Text -> [TraitId] -> Handler Int
runQueue name (t:ts) = do
    nts <- checkTraitStep t
    found <- runQueue name $ ts <> nts
    return $ length nts + found
runQueue name _ = do
  $(logInfo) $ "Done with queue for " <> name
  return 0

checkTraitStep :: TraitId -> Handler [TraitId]
checkTraitStep _id = do
   t' <- runDB $ get _id
   case t' of
     Nothing -> do
       $(logError) $ "Could not find trait " <> encodeText _id
       return []
     Just t -> checkRelevantTheorems t

checkRelevantTheorems :: Trait -> Handler [TraitId]
checkRelevantTheorems trait = do
  pairs <- relevantTheorems trait
  let implications = map snd pairs
  tmap <- spaceTraitMap (traitSpaceId trait) (unionN . map implicationProperties $ implications)
  let proofs = concatMap (\(tid,i) -> apply' tid i tmap) pairs
  mids <- mapM (addProof . traitSpaceId $ trait) proofs
  return $ catMaybes mids

checkTheoremStep :: TheoremId -> Handler [TraitId]
checkTheoremStep _id = do
  t' <- runDB $ get _id
  case t' of
    Nothing -> do
      $(logError) $ "Could not find theorem " <> encodeText _id
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
