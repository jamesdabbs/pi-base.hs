{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module Handlers
  ( module Handlers.Types
  , search
  , assertTrait
  ) where

import Base

import Actions
import qualified Logic as L
import Util

import Handlers.Helpers
import Handlers.Types


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
