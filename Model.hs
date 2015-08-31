{-# LANGUAGE FlexibleInstances #-}
module Model where

import Prelude
import Yesod

import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import Database.Persist.Quasi

import Logic.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

type TraitMap p = Map p (TraitId, TValueId)
