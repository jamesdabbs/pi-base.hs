{-# LANGUAGE OverloadedStrings #-}
module Config
  ( mkLogger
  , mkPool
  ) where

import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Database.Persist.Postgresql          (ConnectionPool, ConnectionString,
                                             createPostgresqlPool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)

import Types

mkLogger :: Environment -> Middleware
mkLogger Test = id
mkLogger Development = logStdoutDev
mkLogger Production = logStdout

mkPool :: Environment -> IO ConnectionPool
mkPool e = case e of
  Test        ->     runNoLoggingT $ createPostgresqlPool (connStr e) (envPool e)
  Development ->     runNoLoggingT $ createPostgresqlPool (connStr e) (envPool e)
  _           -> runStdoutLoggingT $ createPostgresqlPool (connStr e) (envPool e)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr Test = "host=localhost dbname=pi_base_legacy_test user=james port=5432"
connStr    _ = "host=localhost dbname=pi_base_legacy user=james port=5432"
