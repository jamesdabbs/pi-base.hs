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
import Control.Monad (mzero)
import Data.Aeson
import Data.Functor ((<$>))
import Data.Text (Text)
import Data.Vector ((!))
import qualified Data.Set as S

import Model
import Util (unionN)


data Formula a = Atom a Bool
               | And  [Formula a]
               | Or   [Formula a]
               deriving (Eq,Functor)

instance ToJSON a => ToJSON (Formula a) where
  toJSON (Atom p v) = object ["type" .= ("atom"::String), "property" .= p, "value" .= v]
  toJSON (And  sf ) = object ["type" .= ("conjunction"::String), "subformulae" .= sf]
  toJSON (Or   sf ) = object ["type" .= ("disjunction"::String), "subformulae" .= sf]

instance FromJSON a => FromJSON (Formula a) where
  parseJSON (Object v) = do
    klass <- v .: "type"
    case (klass::Text) of
      "atom"        -> Atom <$> v .: "property" <*> v .: "value"
      "conjunction" -> And  <$> v .: "subformulae"
      "disjunction" -> Or   <$> v .: "subformulae"
      _             -> mzero
  parseJSON _ = mzero


data Implication a = Implication (Formula a) (Formula a) deriving Functor

instance FromJSON a => FromJSON (Implication a) where
  parseJSON (Array v) = Implication <$>
    parseJSON (v!0) <*>
    parseJSON (v!1)
  parseJSON _ = mzero

formulaProperties :: (Ord a) => Formula a -> S.Set a
formulaProperties (Atom p _) = S.singleton p
formulaProperties (And  sf ) = unionN . map formulaProperties $ sf
formulaProperties (Or   sf ) = unionN . map formulaProperties $ sf

implicationProperties :: (Ord a) => Implication a -> S.Set a
implicationProperties (Implication a c) =
  (formulaProperties a) `S.union` (formulaProperties c)

data MatchType = Yes | No | Unknown deriving Show
