{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api
  ( API
  , mkApp
  ) where

import Api.Base

import Data.Aeson
import Network.Wai (Application)
import Servant

import Actions (sendLoginEmail, EmailAddress, Host, expireSession, getUniverse)
import Api.Helpers (requireUser)
import Api.Search
import Models (true, false)

import qualified Api.Spaces     as Spaces
import qualified Api.Properties as Properties
import qualified Api.Theorems   as Theorems
import qualified Api.Traits     as Traits


instance ToJSON SearchR where
  toJSON SearchR{..} = toJSON srspaces

type API =
        (  "spaces"     :> Spaces.API
      :<|> "properties" :> Properties.API
      :<|> "theorems"   :> Theorems.API
      :<|> "traits"     :> Traits.API
      :<|> "login"
         :> QueryParam "email" EmailAddress
         :> QueryParam "client_url" Host
         :> POST ()
      :<|> "auth" :> Authenticated :> GET (Entity User)
      :<|> "logout" :> Authenticated :> DELETE ()
      :<|> "universe" :> GET Universe
      :<|> "search"
         :> RequiredParam "q" Text
         :> QueryParam "type" SearchType
         :> DefaultParam "mode" MatchMode "yes" -- TODO: needs a better param name?
         :> GET SearchR
      )

hserve :: ServerT API Handler -> Config -> HandlerContext -> Server API
hserve handlers conf ctx = enter (Nat $ runHandler conf ctx) handlers

server :: Config -> HandlerContext -> Server API
server = hserve $ Spaces.handlers
       :<|> Properties.handlers
       :<|> Theorems.handlers
       :<|> Traits.handlers
       :<|> handlers
       where
         handlers = sendLoginEmail
               :<|> const requireUser
               :<|> expireSession
               :<|> actionToHandler getUniverse
               :<|> search

instance FromText SearchType where
  fromText "properties" = Just ByFormula
  fromText "text"       = Just ByText
  fromText _            = Nothing

instance FromText MatchMode where
  fromText "yes"     = Just Yes
  fromText "no"      = Just No
  fromText "unknown" = Just Unknown
  fromText _         = Nothing

instance FromText TValueId where
  fromText "true"  = Just true
  fromText "false" = Just false
  fromText       _ = Nothing

type XAPI = (WithHandlerContext :> API) :<|> Raw

mkApp :: Config -> Application
mkApp = serve proxy . xserver
  where
    proxy     = Proxy :: Proxy XAPI
    xserver c = server c :<|> serveDirectory "public"
