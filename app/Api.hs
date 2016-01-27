{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- We're deliberately defining the FromText instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api
  ( API
  , server
  ) where

import Base

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Text (unpack)
import Database.Persist
import Servant
import Text.Read (readMaybe)

import Api.Combinators
import Handlers
import Models (true, false)


type API = "status" :> Get '[JSON] HomeR -- TODO: how do you actually route the root? :/
       :<|> "search"
         :> RequiredParam "q" Text
         :> QueryParam "type" SearchType
         :> DefaultParam "mode" MatchMode "yes" -- TODO: needs a better param name?
         :> Get '[JSON] SearchR
       :<|> "properties"
         :> Get '[JSON] PropertiesR
       :<|> "spaces" :> Capture "space_id" SpaceId :> "properties" :> Capture "property_id" PropertyId
         :> QueryParam "value" TValueId
         :> QueryParam "description" Text
         :> Authenticated
         :> Post '[JSON] Trait
       :<|> "traits" :> Capture "trait_id" TraitId
         :> Get '[JSON] Trait


server :: Config -> Server API
server conf = enter (Nat runner) handlers
  where
    handlers = home
          :<|> search
          :<|> allProperties
          :<|> assertTrait
          :<|> showTrait

    runner :: Action v -> EitherT ServantErr IO v
    runner a = runReaderT (runAction a) conf


instance FromText SearchType where
  fromText "properties" = Just ByFormula
  fromText "text"       = Just ByText
  fromText _            = Nothing

instance FromText MatchMode where
  fromText "yes"     = Just Yes
  fromText "no"      = Just No
  fromText "unknown" = Just Unknown
  fromText _         = Nothing

paramToKey :: PersistEntity r => Text -> Maybe (Key r)
paramToKey p = do
  i <- readMaybe $ unpack p
  case keyFromValues [PersistInt64 i] of
    Right key -> Just key
    Left    _ -> Nothing

instance FromText SpaceId where
  fromText = paramToKey

instance FromText PropertyId where
  fromText = paramToKey

instance FromText TraitId where
  fromText = paramToKey

instance FromText TValueId where
  fromText "true"  = Just true
  fromText "false" = Just false
  fromText       _ = Nothing
