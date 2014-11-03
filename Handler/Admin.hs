module Handler.Admin where

import Import

import Control.Monad (filterM, forM_)
import qualified Data.Text as T
import qualified Data.Set as S
import Database.Persist.Sql (rawExecute)

import Explore (checkTheorem)
import Handler.Helpers
import Logic (counterexamples, converse)
import Models

#ifdef DEVELOPMENT
import DB (flushDeductions)
#else
import Network.HTTP.Types (unauthorized401)
#endif

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
postResetR = requireAdmin >> do
#ifdef DEVELOPMENT
  flushDeductions
#else
  error "Can only reset in development mode"
#endif
  returnJson $ object [ "status" .= ("ok" :: Text) ]

postTestResetR :: Handler Value
postTestResetR = requireAdmin >> do
#ifdef DEVELOPMENT
  now <- liftIO getCurrentTime

  runDB $ rawExecute "TRUNCATE struts, assumptions, supporters, proofs, revisions, theorem_properties, traits, properties, spaces, theorems, emails, remote_users RESTART IDENTITY CASCADE" []

  (Entity boolId _) <- runDB . getBy404 $ UValueSetName "boolean"

  _ <- runDB $ do
    insert_ $ User "admin" (Just "admin") True  now now
    insert_ $ User "user"  (Just "user" ) False now now

    forM_ ([1..100] :: [Integer])$ \n -> do
      let n' = T.pack $ show n
      insert_ $ Space ("Space " <> n') (Textarea "-") now now Nothing
      insert_ $ Property ("Property " <> n') [] (Textarea "-") boolId now now

  returnJson $ object [ "status" .= ("Ok" :: Text) ]
#else
  sendResponseStatus unauthorized401 $ object [ "error" .= ("Cannot reset database" :: Text) ]
#endif

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
