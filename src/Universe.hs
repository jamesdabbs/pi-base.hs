module Universe
  ( empty
  , attributes
  , contains
  , insertTrait
  , insertTheorem
  , lookup
  , relevantTheorems
  ) where

import Prelude hiding (lookup)
import Base
import Formula (implicationProperties)

import Control.Monad.State (modify, gets)
import qualified Data.Map  as M
import qualified Data.Set  as S

-- TODO: hide fields of Universe, only use this API

empty :: Universe
empty = Universe M.empty M.empty M.empty

lookup :: SpaceId -> PropertyId -> Universe -> Maybe TValueId
lookup s p = M.lookup p . attributes s

contains :: SpaceId -> PropertyId -> Universe -> Bool
contains sid pid = M.member pid . attributes sid

attributes :: SpaceId -> Universe -> Properties
attributes sid = fromMaybe M.empty . M.lookup sid . uspaces

relevantTheorems :: PropertyId -> Universe -> [(TheoremId, Implication)]
relevantTheorems pid u = z $ map withImplication theoremIds
  where
    theoremIds = M.findWithDefault [] pid $ urelTheorems u
    withImplication tid = (tid, M.lookup tid $ utheorems u)

    z [] = []
    z ((x, Just y) : zs) = (x,y) : z zs
    z (          _ : zs) =         z zs

-- TODO: refactor vvv
insertTrait :: SpaceId -> PropertyId -> TValueId -> State Universe ()
insertTrait s key t = modify $ \u -> u { uspaces = M.alter add s $ uspaces u }
  where
    add :: Maybe Properties -> Maybe Properties
    add mps = Just $ case mps of
      Nothing -> M.fromList [(key,t)]
      Just ps -> M.insert key t ps

insertTheorem :: TheoremId -> Implication -> State Universe ()
insertTheorem tid i = do
  thrmLk <- gets utheorems
  propLk <- gets urelTheorems
  let theorems = M.insert tid i thrmLk
      relThrms = foldl addTid propLk $ S.toList $ implicationProperties i
  modify $ \u -> u { utheorems = theorems, urelTheorems = relThrms }
  where
    addTid :: Map PropertyId [TheoremId] -> PropertyId -> Map PropertyId [TheoremId]
    addTid m p = M.insertWith (++) p [tid] m

