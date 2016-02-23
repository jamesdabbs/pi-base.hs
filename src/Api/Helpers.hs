{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Helpers
  ( halt
  , invalid
  , require
  , withUser
  , get404
  , index
  , serve
  , idFromText
  , getPage
  , revisions
  ) where

import Servant hiding (serve)

import Api.Base
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

invalid :: ByteString -> Action a
invalid msg = halt $ err422 { errBody = msg }

require :: ByteString -> Maybe a -> Action a
require msg mval = maybe (invalid msg) return mval

withUser :: (Entity User -> Action a) -> Entity User -> Action a
withUser a = a
-- withUser f tok = do
--   -- FIXME: proper tokens
--   us <- runDB $ selectList [UserIdent ==. decodeUtf8 tok] []
--   case us of
--     u:_ -> f u
--     _   -> halt $ err403 { errBody = "Invalid token" }

get404 :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Key b -> Action (Entity b)
get404 _id = do
  found <- runDB $ get _id
  case found of
    Nothing -> halt err404
    Just  r -> return $ Entity _id r

index :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => Action [Entity b]
index = runDB $ selectList [] []

serve :: Enter typ (Action :~> EitherT ServantErr IO) ret => typ -> Config -> ret
serve handlers conf = enter (Nat runner) handlers
  where
    runner :: Action v -> EitherT ServantErr IO v
    runner = runA conf

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
  pageResults   <- runDB $ selectList filters allOpts
  pageItemCount <- runDB $ count filters
  let pagePageCount = (quot pageItemCount pagePer) + 1
  return $ Page{..}


getPage :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend)
         => [Filter a] -> Pager a
getPage fs = getPage' fs []

revisions :: Revisable a => Key a -> Pager Revision
revisions key = getPage' (revisionsFor key) [Desc RevisionCreatedAt]
