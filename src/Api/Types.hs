{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Types
  ( module Types
  , Authenticated
  , DefaultParam
  , RequiredParam
  , Paginated
  , GET
  , POST
  , PUT
  , DELETE
  , Body
  , Update
  ) where

import Data.Typeable (Typeable)
import GHC.TypeLits  (Symbol)
import Servant

import Types

data RequiredParam (sym :: Symbol) a deriving Typeable
data DefaultParam  (sym :: Symbol) a (d :: Symbol) deriving Typeable
data Authenticated
type Paginated a = QueryParam "page" Int :> QueryParam "per_page" Int :> GET (Page a)

type GET     = Get     '[JSON]
type POST    = Post    '[JSON]
type PUT     = Put     '[JSON]
type DELETE  = Delete  '[JSON]
type Body    = ReqBody '[JSON]

newtype Update a = Update a
