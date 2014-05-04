module Util
( unionN
, intersectionN
) where

import Prelude

import qualified Data.Set as S


intersectionN :: Ord a => [S.Set a] -> S.Set a
intersectionN [] = S.empty
intersectionN ls = foldl1 S.intersection ls

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl S.union S.empty
