{-# LANGUAGE FlexibleInstances #-}
module Model where

import Prelude
import Yesod

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi

import Logic.Types

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
