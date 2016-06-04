{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import Base

import Control.Concurrent.MVar     (newMVar)
import Configuration.Dotenv        (loadFile)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai                 (Request)
import Network.Wai.Handler.Warp    (run)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy,
                                    simpleMethods, simpleHeaders)
-- import Servant
-- import Servant.JQuery
import System.Environment (lookupEnv)

import Api    (mkApp)
import Config (mkPool, mkLogger, putConf, defaultDatabaseUrl)
import Models (doMigrations, checkBooleans, mkUniverse)

env :: String -> String -> IO String
env k def = do
  mv <- lookupEnv k
  return $ case mv of
    Nothing -> def
    Just  v -> v

reqEnv :: String -> IO String
reqEnv k = do
  mv <- lookupEnv k
  case mv of
    Nothing -> error $ "ENV['" ++ k ++ "'] not set"
    Just  v -> return v

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just simpleCorsResourcePolicy
  { corsMethods        = simpleMethods ++ ["PUT", "DELETE"]
  , corsRequestHeaders = simpleHeaders ++ ["Authorization"]
  , corsOrigins        = Just ([ "http://localhost:8080", "http://localhost:9876" ], True)
  }

main :: IO ()
main = do
  -- writeFile "public/js/api.js" $ jsForAPI (Proxy :: Proxy API)

  getEnv <- read <$> env "ENV" "Development"
  unless (getEnv == Production) $ loadFile True "/data/src/pbr/.env"

  getPort <- read <$> env "PORT" "8081"
  getPool <- env "DATABASE_URL" (defaultDatabaseUrl getEnv) >>= mkPool getEnv

  smtpUsername <- reqEnv "SMTP_USERNAME"
  smtpPassword <- reqEnv "SMTP_PASSWORD"

  universe <- flip runSqlPool getPool $ do
    doMigrations
    checkBooleans
    mkUniverse
  getUVar <- newMVar universe

  conf <- putConf $ Config{..}

  putStrLn $ "Now running on port " ++ show getPort
  run getPort . mkLogger getEnv . cors corsPolicy $ mkApp conf
