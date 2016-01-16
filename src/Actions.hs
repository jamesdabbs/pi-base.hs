module Actions
  ( searchByText
  , searchByFormula
  , getUniverse
  ) where

import Control.Concurrent.STM.TVar (readTVarIO)
import Control.Monad.Reader (liftIO, asks)
import qualified Data.Map as M
import Data.Text (Text)
import Database.Persist

import Types

searchByText :: Text -> Action [Entity Space]
searchByText _ = return []

-- TODO: option for matches / doesn't match / unknown
searchByFormula :: Formula PropertyId -> Action [Entity Space]
searchByFormula f = getUniverse >>= return . flip searchUniverse f

getUniverse :: Action Universe
getUniverse = asks getTU >>= liftIO . readTVarIO

searchUniverse :: Universe -> Formula PropertyId -> [Entity Space]
searchUniverse u f = map fst $ filter (matches f . snd) (uspaces u)

matches :: Formula PropertyId -> M.Map PropertyId Bool -> Bool
matches f pm = True
