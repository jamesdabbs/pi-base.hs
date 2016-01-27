{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Base

import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Data.Int (Int64)
import qualified Data.Map as M
import Data.Text (unpack)
import Database.Persist
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool, SqlPersistT)

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDB :: (MonadReader Config m, Monad m, MonadIO m) => SqlPersistT IO a -> m a
runDB q = asks getPool >>= liftIO . runSqlPool q

mkUniverse :: ReaderT SqlBackend IO Universe
mkUniverse = do
  spaces <- selectKeysList [] []
  pairs  <- mapM traitsFor spaces
  return . Universe $ M.fromList pairs

  where
    traitsFor :: SpaceId -> ReaderT SqlBackend IO (SpaceId, Properties)
    traitsFor _sid = do
      traits <- selectList [TraitSpaceId ==. _sid] []
      let pairs = M.fromList . map (\(Entity _ t) -> (traitPropertyId t, traitValueId t)) $ traits
      return (_sid, pairs)

forceKey :: PersistEntity rec => Int64 -> Key rec
forceKey n = case keyFromValues [PersistInt64 n] of
  Right key -> key
  Left    e -> error $ unpack e

checkBooleans :: ReaderT SqlBackend IO ()
checkBooleans = do
  get true  >>= assertNamed "True"
  get false >>= assertNamed "False"
  where
    assertNamed :: Text -> Maybe TValue -> ReaderT SqlBackend IO ()
    assertNamed name mv = liftIO $ case mv of
      Nothing -> error . unpack $ name <> " not found"
      Just v -> if name == (tValueName v)
        then putStrLn . unpack $ name <> " OK!" -- FIXME
        else error . unpack $ name <> " has the wrong id"

true, false :: TValueId
true  = forceKey 1
false = forceKey 2
