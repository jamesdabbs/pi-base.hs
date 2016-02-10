{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Handlers.Helpers
  ( halt
  , invalid
  , require
  , withUser
  , get404
  , show
  , index
  , serve
  , idFromText
  , getPage
  , revisions
  ) where

import Prelude hiding (show)
import Servant hiding (serve)

import Base
import Control.Monad.Trans.Reader    (runReaderT)
import Control.Monad.Trans.Either    (EitherT)
import Data.Aeson
import Data.ByteString.Lazy          (ByteString)
import Data.Text                     (unpack)
import Database.Persist
import Database.Persist.Sql
import Servant.Server.Internal.Enter (Enter)
import Text.Read                     (readMaybe)

-- These instances need to be defined before defining any of the sub-API types
-- This is an _okay_ place to do that, but I don't love it ...
import Api.Combinators ()

import Models    (runDB)
import Revisions
import Util      (err422)


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

withUser :: (Entity User -> Action a) -> AuthToken -> Action a
withUser f tok = do
  -- FIXME: proper tokens
  us <- runDB $ selectList [UserIdent ==. decodeUtf8 tok] []
  case us of
    u:_ -> f u
    _   -> halt $ err403 { errBody = "Invalid token" }

get404 :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Action (Entity b)
get404 _id = do
  found <- runDB $ get _id
  case found of
    Nothing -> halt err404
    Just  r -> return $ Entity _id r

show :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Action (Entity b)
show = get404

index :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Action [Entity b]
index = runDB $ selectList [] []

serve :: Enter typ (Action :~> EitherT ServantErr IO) ret => typ -> Config -> ret
serve handlers conf = enter (Nat runner) handlers
  where
    runner :: Action v -> EitherT ServantErr IO v
    runner action = runReaderT (runAction action) conf

idFromText :: PersistEntity r => Text -> Maybe (Key r)
idFromText p = do
  i <- readMaybe $ unpack p
  case keyFromValues [PersistInt64 i] of
    Right key -> Just key
    Left    _ -> Nothing

getPage' :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
         => [Filter a] -> [SelectOpt a] -> Pager a
getPage' filters opts mpage mper = do
  let pageNumber    = maybe  1 (max 1)          mpage
      pagePer       = maybe 25 (max 1 . min 50) mper
      offset        = pagePer * (pageNumber - 1)
      allOpts       = opts ++ [LimitTo pagePer, OffsetBy offset]
  pageResults <- runDB $ selectList filters allOpts
  return $ Page{..}


getPage :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
         => [Filter a] -> Pager a
getPage fs = getPage' fs []

revisions :: Revisable a => Key a -> Pager Revision
revisions key = getPage' (revisionsFor key) [Desc RevisionCreatedAt]
