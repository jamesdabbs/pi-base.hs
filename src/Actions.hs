module Actions
  ( searchByText
  , searchByFormula
  ) where

import Data.Text (Text)
import Database.Persist

import Types
import Models

searchByText :: Text -> Action [Entity Space]
searchByText q = return []

-- TODO: option for matches / doesn't match / unknown
searchByFormula :: Formula PropertyId -> Action [Entity Space]
searchByFormula f = return []
