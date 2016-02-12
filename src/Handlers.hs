{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module Handlers
  ( module Handlers.Types
  , search
  ) where

import Base
import qualified Data.ByteString.Lazy.Char8 as LBS

import Actions
import Util

import Handlers.Helpers
import Handlers.Types


search :: Text -> Maybe SearchType -> MatchMode -> Action SearchR
search q mst mm = do
  srspaces <- case mst of
    Just ByText -> searchByText q
    _ -> case eitherDecodeText q of
      Left err -> invalid $ "Could not parse a formula from `q` - " <> (LBS.pack err)
      Right  f -> searchByFormula f mm
  return SearchR{..}
