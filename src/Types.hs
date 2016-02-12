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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Types where

import Control.Concurrent.MVar     (MVar)
import Control.Monad               (liftM, mzero)
import Control.Monad.Reader        (MonadReader)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Either  (EitherT)
import Control.Monad.Trans.Reader  (ReaderT)
import Data.Aeson
import Data.ByteString             (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Int                    (Int64)
import Data.Map                    (Map)
import Data.Set                    (Set)
import Data.Text                   (Text)
import Data.Time                   (UTCTime)
import Data.Typeable               (Typeable)
import Database.Persist.Postgresql (Entity)
import Database.Persist.Postgresql (ConnectionPool)
import Database.Persist.Quasi      (lowerCaseSettings)
import Database.Persist.TH         (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)
import GHC.Generics                (Generic)
import GHC.TypeLits                (Symbol)
import Servant

import Util (encodeText, decodeText)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/schema")

data Environment = Development | Test | Production deriving (Eq, Show, Read, Generic)
instance ToJSON Environment where
  toJSON = genericToJSON defaultOptions

data Config = Config
  { getEnv  :: Environment
  , getPool :: ConnectionPool
  , getUVar :: MVar Universe
  }

newtype Action a = Action
  { runAction :: ReaderT Config (EitherT ServantErr IO) a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

data SearchType = ByFormula | ByText deriving (Eq, Show, Generic)

data Formula a = Atom a Bool
               | And  [Formula a]
               | Or   [Formula a]
               deriving (Show, Eq,Functor)

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
  } deriving Show

data Implication = Implication (Formula PropertyId) (Formula PropertyId) Text deriving Show

instance ToJSON Implication where
  toJSON (Implication a c d) = object
    [ "antecedent"  .= a
    , "consequent"  .= c
    , "description" .= d
    ]

data MatchMode = Yes | No | Unknown deriving (Eq, Show, Generic)

type AuthToken = ByteString

-- TheoremId = Nothing indicates that this proof would depend on a
--   particular newly added theorem, which should be clear from context
data Proof' = Proof' Trait TheoremId (Set PropertyId)
type Deductions = [Proof']

type AuthenticatedAction a = AuthToken -> Action a

type GET     = Get     '[JSON]
type POST    = Post    '[JSON]
type PUT     = Put     '[JSON]
type DELETE  = Delete  '[JSON]
type Body    = ReqBody '[JSON]

data RequiredParam (sym :: Symbol) a deriving Typeable
data DefaultParam (sym :: Symbol) a (d :: Symbol) deriving Typeable
data Authenticated

type Paginated a = QueryParam "page" Int :> QueryParam "per_page" Int :> GET (Page a)
type Pager a = Maybe Int -> Maybe Int -> Action (Page a)
data Page a = Page
  { pageResults   :: [Entity a]
  , pageNumber    :: Int
  , pagePer       :: Int
  }

pageJSON :: Text -> (Entity a -> Value) -> Page a -> Value
pageJSON key fmt Page{..} = object
  [ "page" .= object
      [ "number" .= pageNumber
      , "per"    .= pagePer
      ]
  , key .= map fmt pageResults
  ]
