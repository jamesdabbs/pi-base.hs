{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models.Types where

import Data.Int               (Int64)
import Data.Text              (Text)
import Data.Time              (UTCTime)
import Database.Persist       (Key(..))
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH    (share, mkPersist, sqlSettings, mkMigrate, persistFileWith)

import Formula

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/schema")
