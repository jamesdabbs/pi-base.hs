module Logic where

import Import

import qualified Data.Set as S

import DB (matches')
import Logic.Types
import Util (intersectionN, unionN)


-- TODO: don't hardcode this
boolToValueId :: Bool -> TValueId
boolToValueId v = if v
  then (Key . PersistInt64 $ 1)
  else (Key . PersistInt64 $ 2)

-- Find (ids of) spaces matching a formula
matches :: MatchType -> Formula PropertyId -> Handler (S.Set SpaceId)

-- A conjunction is true if all its parts are true and unknown or false if any
--   parts are unknown, respectively
matches Yes (And sf) = intersectionN <$> mapM (matches Yes) sf
matches e (And sf) = unionN <$> mapM (matches e) sf

-- A disjunction is false if all parts or false, unknown or true if any are
matches No (Or sf) = intersectionN <$> mapM (matches No) sf
matches e (Or sf) = unionN <$> mapM (matches e) sf

-- We have to go to the database to find the spaces that match an atom
matches e (Atom p v) = S.fromList <$> matches' p (boolToValueId v) e
