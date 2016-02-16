{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Types
  ( module Models.Types
  -- Config
  , Config(..)
  , Environment(..)
  -- Logic
  , Formula(..)
  , Implication(..)
  , MatchMode(..)
  , Proof'(..)
  , Deductions
  -- Action
  , Action(..)
  , Pager
  , Page(..)
  , SearchType(..)
  , AuthenticatedAction
  , AuthToken
  ) where

import Control.Concurrent.MVar     (MVar)
import Control.Monad.Reader        (MonadReader)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Either  (EitherT)
import Control.Monad.Trans.Reader  (ReaderT)
import Data.ByteString             (ByteString)
import Data.Set                    (Set)
import Database.Persist.Postgresql (Entity)
import Database.Persist.Postgresql (ConnectionPool)
import GHC.Generics                (Generic)
import Servant                     (ServantErr)

import Formula
import Models.Types
import Universe


data Environment = Development | Test | Production deriving (Eq, Show, Read, Generic)

data Config = Config
  { getEnv  :: Environment
  , getPool :: ConnectionPool
  , getUVar :: MVar Universe
  }

newtype Action a = Action
  { runAction :: ReaderT Config (EitherT ServantErr IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data SearchType = ByFormula | ByText deriving (Eq, Show, Generic)

data MatchMode = Yes | No | Unknown deriving (Eq, Show, Generic)

type AuthToken = ByteString

-- TheoremId = Nothing indicates that this proof would depend on a
--   particular newly added theorem, which should be clear from context
data Proof' = Proof' Trait TheoremId (Set PropertyId)
type Deductions = [Proof']

type AuthenticatedAction a = AuthToken -> Action a

type Pager a = Maybe Int -> Maybe Int -> Action (Page a)
data Page a = Page
  { pageResults   :: [Entity a]
  , pageNumber    :: Int
  , pagePer       :: Int
  }

