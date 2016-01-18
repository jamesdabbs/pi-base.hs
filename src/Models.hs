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
import Database.Persist
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool, SqlPersistT)

import Types

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, Monad m, MonadIO m) => SqlPersistT IO a -> m a
runDB q = asks getPool >>= liftIO . runSqlPool q

mkUniverse :: ReaderT SqlBackend IO Universe
mkUniverse = do
  -- TODO: don't query out e.g. description columns
  spaces <- selectList [] []
  pairs  <- mapM traitsFor spaces
  return $ Universe pairs

  where
    traitsFor :: Entity Space -> ReaderT SqlBackend IO (Entity Space, M.Map PropertyId TValueId)
    traitsFor s@(Entity _sid _) = do
      traits <- selectList [TraitSpaceId ==. _sid] []
      let pairs = M.fromList . map (\(Entity _ t) -> (traitPropertyId t, traitValueId t)) $ traits
      return (s, pairs)
