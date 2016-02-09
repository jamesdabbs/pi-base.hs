{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module Handlers
  ( module Handlers.Types
  , home
  , allProperties
  , search
  , assertTrait
  , showTrait
  , createSpace
  ) where

import Base

import Database.Persist

import Models
import Actions
import qualified Logic as L
import Util

import Handlers.Helpers
import Handlers.Types


home :: Action HomeR
home = do
  hrenvironment <- asks getEnv
  let hrstatus  = "ok"
  let hrmessage = "running"
  universe <- getUniverse
  let hrsize = length $ uspaces universe
  return HomeR{..}

allProperties :: Action [Entity Property]
allProperties = do
  prproperties <- runDB $ selectList ([] :: [Filter Property]) []
  return prproperties
  -- return PropertiesR{..}

search :: Text -> Maybe SearchType -> MatchMode -> Action SearchR
search q mst mm = do
  srspaces <- case mst of
    Just ByText -> searchByText q
    _ -> do
      -- TODO: more informative failure message
      f <- require "Could not parse formula from `q`" $ decodeText q
      searchByFormula f mm
  return SearchR{..}

assertTrait :: SpaceId
            -> PropertyId
            -> TValueId
            -> Text
            -> AuthenticatedAction [TraitId]
assertTrait sid pid vid desc = withUser $ \_ -> do
  let trait = Trait sid pid vid desc Nothing Nothing False
  commit $ L.assertTrait trait


showTrait :: TraitId -> Action Trait
showTrait _id = do
  (Entity _ t) <- get404 _id
  return t

createSpace :: Space -> AuthenticatedAction (Entity Space)
createSpace space = withUser $ \_ -> do
  _id <- runDB $ insert space
  return $ Entity _id space
