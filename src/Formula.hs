{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}

module Formula
  ( Formula(..)
  -- TODO: figure out if Implications need text. Don't export this constructor?
  , Implication(..)
  , neg
  , formulaProperties
  , implicationProperties
  ) where

import Control.Monad (liftM)
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import qualified Data.Set as S
import Data.Text (Text, pack)
import Database.Persist (PersistValue(PersistText), PersistField(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Database.Persist.Types (SqlType(SqlString))

import Util (encodeText, eitherDecodeText, decodeText, unionN)


data Formula a = Atom a Bool
               | And  [Formula a]
               | Or   [Formula a]
               deriving (Show,Eq,Functor)

instance ToJSON a => ToJSON (Formula a) where
  toJSON (Atom p v) = object [encodeText p .= v]
  toJSON (And  sf ) = object ["and" .= sf]
  toJSON (Or   sf ) = object ["or" .= sf]

valueIdToBool :: Int -> Bool
valueIdToBool 1 = True
valueIdToBool 2 = False
valueIdToBool n = error $ "can't coerce " ++ (show n) ++ " to boolean"

instance FromJSON a => FromJSON (Formula a) where
  parseJSON = withObject "formula" $ \v -> case HM.toList v of
      ("and", val)  : _ -> liftM And $ parseJSON val
      ("or",  val)  : _ -> liftM  Or $ parseJSON val
      (key, Bool b) : _ ->
        case decodeText key of
          Just k -> return $ Atom k b
          _ -> fail "could not parse with boolean"
      (key, Number valId) : _ ->
        case decodeText key of
          Just k -> return $ Atom k (valueIdToBool $ round valId)
          _ -> fail "could not parse with value id"
      _ -> fail "no constructor match"

data Implication a = Implication (Formula a) (Formula a) Text deriving Show

instance ToJSON a => ToJSON (Implication a) where
  toJSON (Implication a c d) = object
    [ "antecedent"  .= a
    , "consequent"  .= c
    , "description" .= d
    ]

neg :: Formula a -> Formula a
neg (Atom p True)  = Atom p False
neg (Atom p False) = Atom p True
neg (And  fs)      = Or  $ map neg fs
neg (Or   fs)      = And $ map neg fs

formulaProperties :: (Ord a) => Formula a -> S.Set a
formulaProperties (Atom p _) = S.singleton p
formulaProperties (And  sf ) = unionN . map formulaProperties $ sf
formulaProperties (Or   sf ) = unionN . map formulaProperties $ sf

implicationProperties :: Ord a => Implication a -> S.Set a
implicationProperties (Implication a c _) =
  formulaProperties a `S.union` formulaProperties c

instance (FromJSON a, ToJSON a) => PersistField (Formula a) where
  toPersistValue = PersistText . encodeText
  fromPersistValue v = fromPersistValue v >>= \t ->
    case eitherDecodeText t of
      Left err  -> Left $ "Could not decode persist value: " <> (pack err)
      Right val -> Right val

instance (FromJSON a, ToJSON a) => PersistFieldSql (Formula a) where
  sqlType _ = SqlString
