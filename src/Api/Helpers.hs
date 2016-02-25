{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Helpers
  ( halt
  , invalid
  , require
  , requireUser
  , withUser -- TODO: remove? Change to maybeUser?
  , get404
  , index
  , idFromText
  , getPage
  , revisions
  ) where

import Servant

import Api.Base
import Control.Monad.State           (gets, modify)
import Data.ByteString.Lazy          (ByteString)
import Data.Text                     (unpack)
import Database.Persist
import Database.Persist.Sql
import Text.Read                     (readMaybe)

-- These instances need to be defined before defining any of the sub-API types
-- This is an _okay_ place to do that, but I don't love it ...
import Api.Combinators ()

import Actions   (getUserByToken)
import Models    (runDB)
import Revisions
import Util      (err422)

invalid :: ByteString -> Action a
invalid msg = halt $ err422 { errBody = msg }

require :: ByteString -> Maybe a -> Action a
require msg mval = maybe (invalid msg) return mval

setUser :: Entity User -> HandlerContext -> HandlerContext
setUser u ctx = ctx { requestUser = Just u }

requireUser :: Handler (Entity User)
requireUser = gets requestUser >>= maybe fetchAndSet return
  where
    fetchAndSet = do
      u <- gets requestAuthHeader >>= actionToHandler . getUserByToken
      modify (setUser u) -- Cache the lookup in the request state
      return u

withUser :: (Entity User -> Action a) -> Entity User -> Handler a
withUser a u = do
  -- TODO: this is somewhat vestigial; clean it up
  modify (setUser u)
  actionToHandler $ a u

get404 :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Handler (Entity b)
get404 _id = actionToHandler $ do
  found <- runDB $ get _id
  case found of
    Nothing -> halt err404
    Just  r -> return $ Entity _id r

index :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Action [Entity b]
index = runDB $ selectList [] []

idFromText :: PersistEntity r => Text -> Maybe (Key r)
idFromText p = do
  i <- readMaybe $ unpack p
  case keyFromValues [PersistInt64 i] of
    Right key -> Just key
    Left    _ -> Nothing

getPage' :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
         => [Filter a] -> [SelectOpt a] -> Pager a
getPage' filters opts mpage mper = actionToHandler $ do
  let pageNumber    = maybe  1 (max 1)          mpage
      pagePer       = maybe 25 (max 1 . min 50) mper
      offset        = pagePer * (pageNumber - 1)
      allOpts       = opts ++ [LimitTo pagePer, OffsetBy offset]
  pageResults   <- runDB $ selectList filters allOpts
  pageItemCount <- runDB $ count filters
  let pagePageCount = (quot pageItemCount pagePer) + 1
  return $ Page{..}


getPage :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
         => [Filter a] -> Pager a
getPage fs = getPage' fs []

revisions :: Revisable a => Key a -> Pager Revision
revisions key = getPage' (revisionsFor key) [Desc RevisionCreatedAt]
