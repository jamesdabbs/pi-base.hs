{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Pager
  ( pageJSON
  ) where

import Data.Aeson
import Data.Text (Text)
import Database.Persist (Entity)

import Types

pageJSON :: Text -> (Entity a -> Value) -> Page a -> Value
pageJSON key fmt Page{..} = object
  [ "page" .= object
      [ "number" .= pageNumber
      , "per"    .= pagePer
      , "items"  .= pageItemCount
      , "pages"  .= pagePageCount
      ]
  , key .= map fmt pageResults
  ]
