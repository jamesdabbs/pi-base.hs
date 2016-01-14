{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.Aeson (FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql (SqlBackend(..), runMigration, runSqlPool)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)

import Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/schema")

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDB q = asks getPool >>= liftIO . runSqlPool q
