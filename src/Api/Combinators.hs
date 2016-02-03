{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api.Combinators
  ( RequiredParam
  , DefaultParam
  , Authenticated
  ) where

import Base

import Data.Aeson              (encode)
import Data.String.Conversions (cs)
import Data.Typeable           (Typeable)
import GHC.TypeLits            (Symbol, KnownSymbol, symbolVal)
import Network.HTTP.Types      (parseQueryText, mkStatus, Status, status401)
import Network.Wai             (rawQueryString, responseLBS, Response, requestHeaders)
import Servant
import Servant.Server.Internal (succeedWith, RouteResult)

import Util (err422)

status422 :: Status
status422 = mkStatus 422 "Invalid"

halt :: Status -> ServantErr -> RouteResult Response
halt stat err = succeedWith $ responseLBS stat [] $ encode err

invalid :: Text -> RouteResult Response
invalid msg = halt status422 $ err422 { errBody = cs msg }

data RequiredParam (sym :: Symbol) a deriving Typeable

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (RequiredParam sym a :> sublayout) where

  type ServerT (RequiredParam sym a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
    case lookup paramname querytext of
      Just Nothing  -> stop $ paramname <> " is required"
      Just (Just v) -> case fromText v of
        Nothing -> stop $ paramname <> " could not be parsed"
        Just p  -> continue p
      _ -> stop $ paramname <> " is required"
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          stop = respond . invalid
          continue val = route (Proxy :: Proxy sublayout) (subserver val) request respond



-- TODO: extract and de-dup these
data DefaultParam (sym :: Symbol) a (d :: Symbol) deriving Typeable

instance (KnownSymbol sym, KnownSymbol d, FromText a, HasServer sublayout)
      => HasServer (DefaultParam sym a d :> sublayout) where

  type ServerT (DefaultParam sym a d :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
    case lookup paramname querytext of
      Just (Just v) -> case fromText v of
        Nothing -> stop $ paramname <> " could not be parsed"
        Just p  -> continue p
      _ -> case fromText def of
        -- TODO: this is a dumb failure mode vvv
        Nothing -> stop $ "Could not parse " <> def <> " for " <> paramname
        Just p  -> continue p
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          def = cs $ symbolVal (Proxy :: Proxy d)
          stop = respond . invalid
          continue val = route (Proxy :: Proxy sublayout) (subserver val) request respond


data Authenticated

instance HasServer a => HasServer (Authenticated :> a) where
  type ServerT (Authenticated :> a) m = AuthToken -> ServerT a m

  route Proxy sub request respond =
    case lookup "Authorization" (requestHeaders request) of
      Nothing  -> respond $ halt status401 $ err401 { errBody = "Authentication Required" }
      Just tok -> route (Proxy :: Proxy a) (sub tok) request respond
