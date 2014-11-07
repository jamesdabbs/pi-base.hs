module Handler.Admin where

import Import

import qualified Data.Set as S

import Explore (checkTheorem)
import Handler.Helpers
import Logic (counterexamples, converse)
import Models

import DB (flushDeductions)
import Network.HTTP.Types (unauthorized401)

postExploreR :: Handler Value
postExploreR = requireAdmin >> do
  before <- runDB $ count ([] :: [Filter Trait])
  theorems <- runDB $ selectList [] []
  mapM_ (checkTheorem "Checking all theorems" . entityKey) theorems
  after <- runDB $ count ([] :: [Filter Trait])
  returnJson $ object [ "found" .= (after - before) ]

getContradictionsR :: Handler Value
getContradictionsR = requireAdmin >> do
  theorems <- runDB $ selectList [] []
  counters <- mapM (counterexamples . theoremImplication . entityVal) $ theorems
  let pairs = filter (not . S.null . snd) $ zip theorems counters
  -- properties <- theoremPrefetch . map (entityVal . fst) $ pairs
  returnJson $ object [ "found" .= pairs ]

postResetR :: Handler Value
postResetR = developmentOnly $ do
  flushDeductions
  returnJson $ object [ "status" .= ("ok" :: Text) ]

progressRow :: Entity Space -> Handler Value
progressRow (Entity _id s) = do
  known <- runDB $ count [TraitSpaceId ==. _id]
  unproven <- runDB $ count [TraitSpaceId ==. _id, TraitDeduced ==. False, TraitDescription ==. Textarea ""]
  returnJson $ object
    [ "id" .= _id
    , "name" .= spaceName s
    , "known" .= known
    , "unproven" .= unproven
    ]

getTraitProgressR :: Handler Value
getTraitProgressR = requireAdmin >> do
  spaces <- runDB $ selectList [] [Asc SpaceName]
  vals <- sequence . map progressRow $ spaces -- FIXME: n+1
  returnJson vals

theoremUnknownReversables :: Handler [Entity Theorem]
theoremUnknownReversables = do
  ts <- runDB $ selectList [] [Asc TheoremId]
  -- n+1
  filterM interesting $ ts
  where
    interesting (Entity _id t) = do
      if null . theoremConverseIds $ t
        then do
          cxs <- counterexamples . converse . theoremImplication $ t
          return . S.null $ cxs
        else return False

getTheoremProgressR :: Handler Value
getTheoremProgressR = requireAdmin >> do
  unprovenTheorems <- runDB $ selectList [TheoremDescription ==. Textarea ""] [Asc TheoremId]
  reversables <- theoremUnknownReversables
  returnJson $ object
    [ "unproven" .= unprovenTheorems
    , "unknown_reversables" .= reversables
    ]
