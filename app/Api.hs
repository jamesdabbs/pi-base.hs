{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( API
  , handlers
  ) where

import Data.Text (Text)
import Servant

import Types
import Handlers

type API = "status" :> Get '[JSON] HomeR -- TODO: how do you actually route the root? :/
       :<|> "search"
         :> QueryParam "q" Text
         :> QueryParam "type" SearchType
         :> Get '[JSON] SearchR
       :<|> "properties"
         :> Get '[JSON] PropertiesR


handlers = home
      :<|> search
      :<|> allProperties

instance FromText SearchType where
  fromText "properties" = Just ByFormula
  fromText "text"       = Just ByText
  fromText _            = Nothing
