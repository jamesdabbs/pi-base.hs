{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Combinators
  ( RequiredParam
  , DefaultParam
  , Authenticated
  ) where

import Api.Types
import Base
import Control.Lens            ((&), (<>~))
import Data.Aeson              (encode)
import qualified Data.ByteString.Char8 as BS
import Data.String.Conversions (cs)
import GHC.TypeLits            (KnownSymbol, symbolVal)
import Network.HTTP.Types      (parseQueryText, mkStatus, Status, status401)
import Network.Wai             (rawQueryString, responseLBS, Response, requestHeaders)
import Servant
import Servant.JQuery
import Servant.Server.Internal (succeedWith, RouteResult)

import Api.Base (unsafeRunA)
import Actions  (getUserByToken)
import Util     (err422)

halt :: ServantErr -> RouteResult Response
halt e = succeedWith $ responseLBS stat [] $ encode e
  where
    stat = mkStatus (errHTTPCode e) (BS.pack $ errReasonPhrase e)

invalid :: Text -> RouteResult Response
invalid msg = halt $ err422 { errBody = cs msg }


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

instance (KnownSymbol sym, FromText a, HasJQ sublayout)
      => HasJQ (RequiredParam sym a :> sublayout) where
  type JQ (RequiredParam sym a :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)



-- TODO: extract and de-dup these

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

instance (KnownSymbol sym, KnownSymbol d, FromText a, HasJQ sublayout)
      => HasJQ (DefaultParam sym a d :> sublayout) where
  type JQ (DefaultParam sym a d :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor (Proxy :: Proxy sublayout) $
      req & reqUrl.queryStr <>~ [QueryArg str Flag]

    where str = symbolVal (Proxy :: Proxy sym)


instance HasServer a => HasServer (Authenticated :> a) where
  type ServerT (Authenticated :> a) m = Entity User -> ServerT a m

  route Proxy sub request respond = do
    let token = lookup "Authorization" (requestHeaders request)
    result <- unsafeRunA $ getUserByToken token
    case result of
      Left err -> respond $ halt err
      Right user -> route (Proxy :: Proxy a) (sub user) request respond

instance (HasJQ sublayout) => HasJQ (Authenticated :> sublayout) where
  type JQ (Authenticated :> sublayout) = JQ sublayout

  jqueryFor Proxy req =
    jqueryFor subP (req & reqHeaders <>~ [HeaderArg "Authorization"])

    where subP = Proxy :: Proxy sublayout
