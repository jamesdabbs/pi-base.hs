{-# LANGUAGE OverloadedStrings #-}
module Config
  ( mkLogger
  , mkPool
  , putConf
  , getConf
  , defaultDatabaseUrl
  ) where

import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Data.IORef                           (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Database.Persist.Postgresql          (ConnectionPool, ConnectionString,
                                             createPostgresqlPool)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import System.IO.Unsafe                     (unsafePerformIO)
import Web.Heroku                           (parseDatabaseUrl)

import Base

mkLogger :: Environment -> Middleware
mkLogger Test = id
mkLogger Development = logStdoutDev
mkLogger Production = logStdout

urlToConnString :: String -> ConnectionString
urlToConnString = normalize . parseDatabaseUrl
  where normalize = encodeUtf8 . T.unwords . map (\(k,v) -> T.concat [k, "='", v, "'"])

mkPool :: Environment -> String -> IO ConnectionPool
mkPool e url = case e of
  Test        -> runNoLoggingT $ createPostgresqlPool connStr (envPool e)
  Development -> runNoLoggingT $ createPostgresqlPool connStr (envPool e)
  _ ->       runStdoutLoggingT $ createPostgresqlPool connStr (envPool e)
  where
    connStr = urlToConnString url

envPool :: Environment -> Int
envPool Production = 8
envPool          _ = 1

defaultDatabaseUrl :: Environment -> String
defaultDatabaseUrl e = case e of
  Test        -> root ++ "pi_base_legacy_test"
  Development -> root ++ "pi_base_legacy"
  Production  -> error "Refusing to default DB connection in Production"
  where
    root = "postgres://james:@localhost:5432/"

confRef :: IORef (Maybe Config)
{-# NOINLINE confRef #-}
confRef = unsafePerformIO $ newIORef Nothing

putConf :: Config -> IO ()
putConf c = do
  mc <- readIORef confRef
  case mc of
    Nothing  -> return ()
    Just old -> case getEnv old of
      Production -> error "Refusing to overwrite global Config cache"
      _          -> putStrLn "*** WARNING: overwriting global Config cache"
  writeIORef confRef $ Just c

-- This assumes that `putConf` has been called previously (as it is on app boot)
getConf :: IO Config
getConf = do
  mc <- readIORef confRef
  case mc of
    Nothing -> error "Use `putConf` to set the global config"
    Just c  -> return c
