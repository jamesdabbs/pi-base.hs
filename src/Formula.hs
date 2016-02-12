{-# LANGUAGE OverloadedStrings          #-}
module Formula
  ( neg
  , formulaProperties
  , implicationProperties
  , (==.)
  , (&&.)
  , (||.)
  , (=>.)
  , (~.)
  , (|=)
  , true
  , false
  ) where

import qualified Data.Set as S

import Base
import Util (unionN, toSqlKey)

true, false :: TValueId
true  = toSqlKey 1
false = toSqlKey 2

neg :: Formula a -> Formula a
neg (Atom p True)  = Atom p False
neg (Atom p False) = Atom p True
neg (And  fs)      = Or  $ map neg fs
neg (Or   fs)      = And $ map neg fs

formulaProperties :: (Ord a) => Formula a -> Set a
formulaProperties (Atom p _) = S.singleton p
formulaProperties (And  sf ) = unionN . map formulaProperties $ sf
formulaProperties (Or   sf ) = unionN . map formulaProperties $ sf

implicationProperties :: Implication -> Set PropertyId
implicationProperties (Implication a c _) =
  formulaProperties a `S.union` formulaProperties c


-- The following helpers are primarily intended to allow more expressive specs
--   and probably shouldn't be used in application code

fromBool :: Bool -> TValueId
fromBool True = true
fromBool    _ = false

(==.) :: Int64 -> Bool -> Formula PropertyId
(==.) p v = Atom (toSqlKey p) v

(&&.) :: Formula a -> Formula a -> Formula a
(&&.) f (And sf) = And (f:sf)
(&&.) (And sf) f = And (f:sf)
(&&.) f1 f2 = And [f1, f2]

(||.) :: Formula a -> Formula a -> Formula a
(||.) f (Or sf) = Or (f:sf)
(||.) (Or sf) f = Or (f:sf)
(||.) f1 f2 = Or [f1, f2]

(~.) :: (Int64, Int64) -> Bool -> Trait
(~.) (sid,pid) tf = Trait (toSqlKey sid) (toSqlKey pid) (fromBool tf) "" False

(=>.) :: Formula PropertyId -> Formula PropertyId -> Implication
(=>.) a c = Implication a c ""

(|=) :: SpaceId -> PropertyId -> Trait
(|=) s p = Trait s p true "" False
