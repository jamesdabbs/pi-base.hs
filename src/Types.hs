{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad (liftM, mzero)
import Control.Monad.Reader (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Database.Persist.Postgresql (ConnectionPool)
import GHC.Generics (Generic)
import Servant (ServantErr)

import Util (encodeText, decodeText)

data Environment = Development | Test | Production deriving (Eq, Show, Read, Generic)
instance ToJSON Environment

data Config = Config
  { getEnv :: Environment
  , getPool :: ConnectionPool
  }

newtype Action a = Action
  { runAction :: ReaderT Config (EitherT ServantErr IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data SearchType = ByFormula | ByText deriving (Eq, Show, Generic)

data Formula a = Atom a Bool
               | And  [Formula a]
               | Or   [Formula a]
               deriving (Eq,Functor)

instance ToJSON a => ToJSON (Formula a) where
  toJSON (Atom p v) = object [encodeText p .= v]
  toJSON (And  sf ) = object ["and" .= sf]
  toJSON (Or   sf ) = object ["or" .= sf]

instance FromJSON a => FromJSON (Formula a) where
  parseJSON (Object v) = case HM.toList v of
      ("and", val)  : _ -> liftM And $ parseJSON val
      ("or",  val)  : _ -> liftM  Or $ parseJSON val
      (key, Bool b) : _ ->
        case decodeText key of
          Just k -> return $ Atom k b
          _      -> mzero
      _ -> mzero
  parseJSON _ = mzero
