{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Types
  ( module Types
  , HandlerContext(..)
  , Handler(..)
  , Authenticated
  , DefaultParam
  , RequiredParam
  , Paginated
  , WithHandlerContext
  , GET
  , POST
  , PUT
  , DELETE
  , Body
  , Pager
  ) where

import Data.Typeable (Typeable)
import GHC.TypeLits  (Symbol)
import Servant

import Base
import Types

data HandlerContext = HandlerContext
  { requestAuthHeader :: Maybe AuthToken
  , requestUser       :: Maybe (Entity User)
  }

newtype Handler a = Handler
  { unHandler :: StateT HandlerContext Action a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadState HandlerContext)

data RequiredParam (sym :: Symbol) a deriving Typeable
data DefaultParam  (sym :: Symbol) a (d :: Symbol) deriving Typeable
data Authenticated
type Paginated a = QueryParam "page" Int :> QueryParam "per_page" Int :> GET (Page a)
data WithHandlerContext

type GET     = Get     '[JSON]
type POST    = Post    '[JSON]
type PUT     = Put     '[JSON]
type DELETE  = Delete  '[JSON]
type Body    = ReqBody '[JSON]

type Pager a = Maybe Int -> Maybe Int -> Handler (Page a)
