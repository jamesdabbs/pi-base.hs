{-# LANGUAGE OverloadedStrings #-}
module Spec.Helpers where

import Base
import Models (true, false)
import Util (toSqlKey)

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

(=>.) :: Formula a -> Formula a -> Implication a
(=>.) a c = Implication a c ""

(|=) :: SpaceId -> PropertyId -> Trait
(|=) s p = Trait s p true "" False
