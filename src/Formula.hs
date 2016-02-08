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
  , true
  , false
  ) where

import Data.Int (Int64)
import qualified Data.Set as S

import Base
import Util (unionN, forceKey)

true, false :: TValueId
true  = forceKey 1
false = forceKey 2

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
implicationProperties (Implication a c) =
  formulaProperties a `S.union` formulaProperties c


-- The following helpers are primarily intended to allow more expressive specs
--   and probably shouldn't be used in application code

(==.) :: Int64 -> Bool -> Formula PropertyId
(==.) p v = Atom (forceKey p) v

(&&.) :: Formula a -> Formula a -> Formula a
(&&.) f (And sf) = And (f:sf)
(&&.) (And sf) f = And (f:sf)
(&&.) f1 f2 = And [f1, f2]

(||.) :: Formula a -> Formula a -> Formula a
(||.) f (Or sf) = Or (f:sf)
(||.) (Or sf) f = Or (f:sf)
(||.) f1 f2 = Or [f1, f2]

(~.) :: (Int64, Int64) -> Bool -> Trait
(~.) (sid,pid) tf = Trait (forceKey sid) (forceKey pid) tv "" Nothing Nothing False
  where
    tv = if tf then true else false

(=>.) :: Formula PropertyId -> Formula PropertyId -> Implication
(=>.) = Implication
