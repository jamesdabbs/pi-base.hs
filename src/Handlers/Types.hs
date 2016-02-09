{-# LANGUAGE TemplateHaskell   #-}
module Handlers.Types
  ( HomeR(..)
  , SearchR(..)
  ) where

import Base

import Data.Aeson.TH

data HomeR = HomeR
  { hrenvironment :: Environment
  , hrstatus :: Text
  , hrmessage :: Text
  , hrsize :: Int
  } deriving (Show)
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''HomeR)

data SearchR = SearchR
  { srspaces :: [Entity Space]
  }

