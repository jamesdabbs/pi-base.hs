{-# LANGUAGE OverloadedStrings #-}
module Config
  ( mkLogger
  , mkPool
  , putConf
  , getConf
  ) where

import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Data.IORef                           (IORef, newIORef, readIORef, writeIORef)
import Database.Persist.Postgresql          (ConnectionPool, ConnectionString,
                                             createPostgresqlPool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import System.IO.Unsafe                     (unsafePerformIO)

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

confRef :: IORef (Maybe Config)
{-# NOINLINE confRef #-}
confRef = unsafePerformIO $ newIORef Nothing

putConf :: Config -> IO ()
putConf c = do
  mc <- readIORef confRef
  case mc of
    Nothing -> putStrLn "*** WARNING: overwriting stored Config"
    _       -> return ()
  writeIORef confRef $ Just c

-- This assumes that `putConf` has been called previously (as it is on app boot)
getConf :: IO Config
getConf = do
  mc <- readIORef confRef
  case mc of
    Nothing -> error "Use `putConf` to set the global config"
    Just c  -> return c
