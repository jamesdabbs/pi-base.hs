{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Reader (MonadReader, ReaderT, asks, liftIO)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as M
import Database.Persist (selectList)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool, SqlPersistT)

import Types

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, Monad m, MonadIO m) => SqlPersistT IO a -> m a
runDB q = asks getPool >>= liftIO . runSqlPool q

mkUniverse :: ReaderT SqlBackend IO Universe
mkUniverse = do
  spaces <- selectList [] []
  let pairs = map (\s -> (s, M.empty)) spaces -- TODO
  return $ Universe pairs
