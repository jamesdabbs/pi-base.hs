{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- We're deliberately defining the FromText instances here
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api
  ( API
  , mkApp
  ) where

import Base

import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson
import Data.Text                  (unpack)
import Database.Persist
import Network.Wai                (Application)
import Servant
import Text.Read                  (readMaybe)

import Api.Combinators
import Formula (true, false)
import Handlers
import Handlers.Resource


instance ToJSON [Entity Space] where
  toJSON ps = object [ "spaces" .= map fmt ps ]
    where
      fmt (Entity _id Space{..}) = object
        [ "id" .= _id
        , "name" .= spaceName
        ]

instance ToJSON (Entity Space) where
  toJSON (Entity _id Space{..}) = object
    [ "id"                .= _id
    , "name"              .= spaceName
    , "description"       .= spaceDescription
    , "proof_of_topology" .= spaceProofOfTopology
    ]

instance ToJSON [Entity Property] where
  toJSON ps = object [ "properties" .= map fmt ps ]
    where
      fmt (Entity _id Property{..}) = object
        [ "id"   .= _id
        , "name" .= propertyName
        ]

instance ToJSON (Entity Property) where
  toJSON (Entity _id Property{..}) = object
    [ "id"                .= _id
    , "name"              .= propertyName
    , "description"       .= propertyDescription
    ]

instance ToJSON SearchR where
  toJSON SearchR{..} = toJSON srspaces

type API = "status" :> Get '[JSON] HomeR -- TODO: how do you actually route the root? :/
       :<|> "search"
         :> RequiredParam "q" Text
         :> QueryParam "type" SearchType
         :> DefaultParam "mode" MatchMode "yes" -- TODO: needs a better param name?
         :> Get '[JSON] SearchR
       :<|> "properties"
         :> Get '[JSON] [Entity Property]
       :<|> "spaces" :> Capture "space_id" SpaceId :> "properties" :> Capture "property_id" PropertyId
         :> RequiredParam "value" TValueId
         :> RequiredParam "description" Text
         :> Authenticated
         :> Post '[JSON] [TraitId]
       :<|> "traits" :> Capture "trait_id" TraitId
         :> Get '[JSON] Trait

       :<|> "spaces"     :> Resource Space
       :<|> "properties" :> Resource Property

server :: Config -> Server API
server conf = enter (Nat runner) handlers
  where
    handlers = home
          :<|> search
          :<|> allProperties
          :<|> assertTrait
          :<|> showTrait
          :<|> resource (Proxy :: Proxy Space)
          :<|> resource (Proxy :: Proxy Property)

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

type ExtAPI = API :<|> Raw

xserver :: Config -> Server ExtAPI
xserver conf = server conf :<|> serveDirectory "public"

mkApp :: Config -> Application
mkApp = serve xapi . xserver
  where
    xapi :: Proxy ExtAPI
    xapi = Proxy
