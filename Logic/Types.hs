{-# LANGUAGE DeriveFunctor #-}
module Logic.Types
( Formula (..)
, Implication (..)
, MatchType (..)
, formulaProperties
, implicationProperties
) where

import Prelude

import Control.Applicative ((<*>))
import Control.Monad (liftM, mzero)
import Data.Aeson
import Data.Functor ((<$>))
import Data.List (intercalate)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Vector ((!))
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Database.Persist.Types (SqlType (SqlString))

import DB.Serialization (dump, load)
import Util (unionN)


data Formula a = Atom a Bool
               | And  [Formula a]
               | Or   [Formula a]
               deriving (Eq,Functor)

instance Show a => Show (Formula a) where
  show (Atom p True ) = show p
  show (Atom p False) = "¬"++ (show p)
  show (And  fs     ) = "(" ++ (intercalate " & " . map show $ fs) ++ ")"
  show (Or   fs     ) = "(" ++ (intercalate " | " . map show $ fs) ++ ")"

instance ToJSON a => ToJSON (Formula a) where
  toJSON (Atom p v) = object ["_type" .= ("atom"::String), "property" .= p, "value" .= v]
  toJSON (And  sf ) = object ["_type" .= ("conjunction"::String), "subformulae" .= sf]
  toJSON (Or   sf ) = object ["_type" .= ("disjunction"::String), "subformulae" .= sf]

valueIdToBool :: Int -> Bool
valueIdToBool 1 = True
valueIdToBool 2 = False
valueIdToBool _ = error "Unrecognized value"

instance FromJSON a => FromJSON (Formula a) where
  parseJSON (Object v) = do
    klass <- v .: "_type"
    case (klass::Text) of
      "atom"        -> Atom <$> v .: "property" <*> liftM valueIdToBool (v .: "value")
      "conjunction" -> And  <$> v .: "subformulae"
      "disjunction" -> Or   <$> v .: "subformulae"
      _             -> mzero
  parseJSON _ = mzero

instance (FromJSON a, ToJSON a) => PersistField (Formula a) where
  toPersistValue = dump
  fromPersistValue = load

instance (FromJSON a, ToJSON a) => PersistFieldSql (Formula a) where
  sqlType _ = SqlString


data Implication a = Implication (Formula a) (Formula a) deriving Functor

instance Show a => Show (Implication a) where
  show (Implication a c) = show a ++ " ⇒ " ++ show c

instance FromJSON a => FromJSON (Implication a) where
  parseJSON (Array v) = Implication <$>
    parseJSON (v!0) <*>
    parseJSON (v!1)
  parseJSON _ = mzero

data MatchType = Yes | No | Unknown deriving Show


formulaProperties :: (Ord a) => Formula a -> S.Set a
formulaProperties (Atom p _) = S.singleton p
formulaProperties (And  sf ) = unionN . map formulaProperties $ sf
formulaProperties (Or   sf ) = unionN . map formulaProperties $ sf

implicationProperties :: (Ord a) => Implication a -> S.Set a
implicationProperties (Implication a c) =
  (formulaProperties a) `S.union` (formulaProperties c)
