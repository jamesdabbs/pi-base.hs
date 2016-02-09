{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad               (liftM, mzero)
import Control.Monad.Reader        (MonadReader)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Either  (EitherT)
import Control.Monad.Trans.Reader  (ReaderT)
import Data.Aeson
import Data.ByteString             (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Map                    (Map)
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import Database.Persist            (Entity(..))
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Quasi      (lowerCaseSettings)
import Database.Persist.TH         (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import GHC.Generics                (Generic)
import Servant                     (ServantErr)

import Util (encodeText, decodeText, forceKey)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/schema")

data Environment = Development | Test | Production deriving (Eq, Show, Read, Generic)
instance ToJSON Environment where
  toJSON = genericToJSON defaultOptions

data Config = Config
  { getEnv  :: Environment
  , getPool :: ConnectionPool
  , getUVar :: TVar Universe
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

type Properties = Map PropertyId TValueId

data Universe = Universe
  { uspaces      :: Map SpaceId Properties
  , utheorems    :: Map TheoremId Implication
  , urelTheorems :: Map PropertyId [TheoremId]
  }

data Implication = Implication (Formula PropertyId) (Formula PropertyId)

data MatchMode = Yes | No | Unknown deriving (Eq, Show, Generic)

type AuthToken = ByteString

instance FromJSON Space where
  parseJSON = withObject "space" $ \o -> do
    spaceName        <- o .: "name"
    spaceDescription <- o .: "description"
    let spaceCreatedAt       = Nothing
        spaceUpdatedAt       = Nothing
        spaceProofOfTopology = Nothing
    return Space{..}

instance FromJSON Property where
  parseJSON = withObject "property" $ \o -> do
    propertyName        <- o .: "name"
    propertyDescription <- o .: "description"
    let propertyCreatedAt  = Nothing
        propertyUpdatedAt  = Nothing
        propertyValueSetId = forceKey 1
        propertyAliases    = ""
    return Property{..}

-- Proved property, used theorem, assumed properties
data Proof' = Proof' PropertyId TheoremId (Set PropertyId)

type AuthenticatedAction a = AuthToken -> Action a
