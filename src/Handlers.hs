{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handlers
  ( HomeR
  , PropertiesR
  , SearchR
  , home
  , allProperties
  , search
  , assertTrait
  , showTrait
  , createSpace
  ) where

import Base

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.STM (atomically)
import Control.Monad.State (execState)
import Data.Aeson (encode)
import Data.Aeson.TH
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Database.Persist
import Database.Persist.Sql
import Servant

import Models
import Actions
import qualified Logic as L
import Util
import qualified Universe as U

data HomeR = HomeR
  { hrenvironment :: Environment
  , hrstatus :: Text
  , hrmessage :: Text
  , hrsize :: Int
  } deriving (Show)
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''HomeR)

data PropertiesR = PropertiesR
  { prproperties :: [Entity Property]
  }
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''PropertiesR)

data SearchR = SearchR
  { srspaces :: [Entity Space]
  }
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 2} ''SearchR)

halt :: ServantErr -> Action a
halt err = Action . lift . left $ err'
  where
    err' = err
      { errBody    = encode err
      , errHeaders = [("Content-Type", "application/json")]
      }

invalid :: ByteString -> Action a
invalid msg = halt $ err422 { errBody = msg }

require :: ByteString -> Maybe a -> Action a
require msg mval = maybe (invalid msg) return mval

home :: Action HomeR
home = do
  hrenvironment <- asks getEnv
  let hrstatus  = "ok"
  let hrmessage = "running"
  universe <- getUniverse
  let hrsize = length $ uspaces universe
  return HomeR{..}

allProperties :: Action PropertiesR
allProperties = do
  prproperties <- runDB $ selectList ([] :: [Filter Property]) []
  return PropertiesR{..}

search :: Text -> Maybe SearchType -> MatchMode -> Action SearchR
search q mst mm = do
  srspaces <- case mst of
    Just ByText -> searchByText q
    _ -> do
      -- TODO: more informative failure message
      f <- require "Could not parse formula from `q`" $ decodeText q
      searchByFormula f mm
  return SearchR{..}

withUser :: (Entity User -> Action a) -> AuthToken -> Action a
withUser f tok = do
  -- FIXME: proper tokens
  us <- runDB $ selectList [UserIdent ==. decodeUtf8 tok] []
  case us of
    u:_ -> f u
    _   -> halt $ err403 { errBody = "Invalid token" }

commit :: State Universe [Proof'] -> Action [TraitId]
commit modfications = do
  uvar <- asks getUVar
  liftIO . atomically . modifyTVar uvar $ execState modifications
  -- TODO: persist proofs to DB
  --       what about Universe state if something fails here vvv ?
  return []

assertTrait :: SpaceId -> PropertyId -> Maybe TValueId -> Maybe Text -> AuthToken -> Action Trait
assertTrait sid pid mvid mdesc = withUser $ \user -> do
  vid  <- require "`value` is required" mvid
  desc <- require "`description` is required" mdesc

  let trait = Trait sid pid vid desc Nothing Nothing False
  proofs <- commit $ L.assertTrait trait

  -- Update universe
  -- Update DB
  -- Check for deductions
  error "Not implemented"

get404 :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Action b
get404 _id = do
  found <- runDB $ get _id
  case found of
    Nothing -> halt err404
    Just  r -> return r

showTrait :: TraitId -> Action Trait
showTrait = get404

createSpace :: Space -> AuthToken -> Action (Entity Space)
createSpace space = withUser $ \(Entity _ user) -> do
  let name = userIdent user
  _id <- runDB $ insert space
  return $ Entity _id space
