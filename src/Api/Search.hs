{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}

module Api.Search
  ( SearchR(..)
  , search
  ) where

import Api.Base
import qualified Data.ByteString.Lazy.Char8 as LBS

import Actions
import Util

import Api.Helpers

data SearchR = SearchR
  { srspaces :: [Entity Space]
  }

search :: Text -> Maybe SearchType -> MatchMode -> Handler SearchR
search q mst mm = actionToHandler $ do
  srspaces <- case mst of
    Just ByText -> searchByText q
    _ -> case eitherDecodeText q of
      Left err -> invalid $ "Could not parse a formula from `q` - " <> (LBS.pack err)
      Right  f -> searchByFormula f mm
  return SearchR{..}
