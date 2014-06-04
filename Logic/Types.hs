{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
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
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Set as S
import Data.Text (unpack)
import Data.Vector ((!))
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Database.Persist.Types (SqlType (SqlString))
import qualified Data.HashMap.Strict as M

import DB.Serialization (dump, load)
import Util (encodeText, unionN)


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
  toJSON (Atom p v) = object [encodeText p .= v]
  toJSON (And  sf ) = object ["and" .= sf]
  toJSON (Or   sf ) = object ["or" .= sf]

valueIdToBool :: Int -> Bool
valueIdToBool 1 = True
valueIdToBool 2 = False
valueIdToBool _ = error "Unrecognized value"

instance FromJSON (Formula Int64) where
  parseJSON (Object v) = do
    case head . M.toList $ v of
      ("and", val) -> do
        ands <- parseJSON val
        return $ And ands
      ("or", val) -> do
        ors <- parseJSON val
        return $ Or ors
      (key, Number valId) -> do
        return $ Atom (read . unpack $ key) (valueIdToBool . round $ valId)
      _ -> mzero
  parseJSON _ = mzero

instance PersistField (Formula Int64) where
  toPersistValue = dump
  fromPersistValue = load

instance PersistFieldSql (Formula Int64) where
  sqlType _ = SqlString


data Implication a = Implication (Formula a) (Formula a) deriving Functor

instance Show a => Show (Implication a) where
  show (Implication a c) = show a ++ " ⇒ " ++ show c

instance FromJSON (Implication Int64) where
  parseJSON (Array v) = Implication <$>
    parseJSON (v!0) <*>
    parseJSON (v!1)
  parseJSON _ = mzero

data MatchType = Yes | No | Unknown deriving (Show, Eq)


formulaProperties :: (Ord a) => Formula a -> S.Set a
formulaProperties (Atom p _) = S.singleton p
formulaProperties (And  sf ) = unionN . map formulaProperties $ sf
formulaProperties (Or   sf ) = unionN . map formulaProperties $ sf

implicationProperties :: (Ord a) => Implication a -> S.Set a
implicationProperties (Implication a c) =
  (formulaProperties a) `S.union` (formulaProperties c)
