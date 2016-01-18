{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handlers
  ( HomeR
  , PropertiesR
  , SearchR
  , home
  , allProperties
  , search
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either (left)
import Control.Monad.Reader (asks)
import Data.Aeson (encode, object, (.=))
import Data.Aeson.TH
import Data.Text (Text)
import Database.Persist
import Servant

import Types
import Models
import Actions
import Util

data HomeR = HomeR
  { hrenvironment :: Environment
  , hrstatus :: Text
  , hrmessage :: Text
  , hrsize :: Int
  } deriving (Show)
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''HomeR)

data PropertiesR = PropertiesR
  { prproperties :: [Entity Property]
  }
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''PropertiesR)

data SearchR = SearchR
  { srspaces :: [Entity Space]
  }
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''SearchR)

halt :: ServantErr -> Text -> Action a
halt err msg = Action . lift . left $ err'
  where
    err' = err
      { errBody    = encode $ object [ "error" .= msg, "status" .= errHTTPCode err ]
      , errHeaders = [("Content-Type", "application/json")]
      }

require :: Text -> Maybe a -> Action a
require msg mval = case mval of
  Just val -> return val
  Nothing  -> halt err400 msg

home :: Action HomeR
home = do
  hrenvironment <- asks getEnv
  let hrstatus  = "ok"
  let hrmessage = "running"
  universe <- getUniverse
  let hrsize = length $ uspaces universe
  return HomeR{..}

allProperties :: Action PropertiesR
allProperties = do
  prproperties <- runDB $ selectList ([] :: [Filter Property]) []
  return PropertiesR{..}

search :: Maybe Text -> Maybe SearchType -> Action SearchR
search mq mt = do
  q <- require "`q` is required" mq
  case mt of
    Just ByText -> do
      srspaces <- searchByText q
      return SearchR{..}
    _ -> do
      -- TODO: more informative failure message
      f <- require "Could not parse formula from `q`" $ decodeText q
      srspaces <- searchByFormula f
      return SearchR{..}