{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}
module Handlers.Helpers
  ( halt
  , invalid
  , require
  , withUser
  , get404
  ) where

import Base
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Database.Persist
import Database.Persist.Sql
import Models (runDB)
import Servant
import Util (err422)


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
