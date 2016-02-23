{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main (main) where

import Base

import Control.Concurrent.MVar (newMVar)
import Database.Persist.Postgresql (runSqlPool)
import Configuration.Dotenv (loadFile)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, CorsResourcePolicy(..), simpleCorsResourcePolicy,
                                    simpleMethods, simpleHeaders)
-- import Servant
-- import Servant.JQuery
import System.Environment (lookupEnv)

import Api    (mkApp)
import Config (mkPool, mkLogger, putConf)
import Models (doMigrations, checkBooleans, mkUniverse)

env :: Read a => String -> a -> IO a
env k def = do
  mv <- lookupEnv k
  return $ case mv of
    Nothing -> def
    Just  v -> read v

reqEnv :: String -> IO String
reqEnv k = do
  mv <- lookupEnv k
  case mv of
    Nothing -> error $ "ENV['" ++ k ++ "'] not set"
    Just  v -> return v

main :: IO ()
main = do
  -- writeFile "public/js/api.js" $ jsForAPI (Proxy :: Proxy API)

  getEnv <- env "ENV" Development
  unless (getEnv == Production) $ loadFile True "/data/src/pbr/.env"

  getPort <- env "PORT" 8081
  getPool <- mkPool getEnv

  smtpUsername <- reqEnv "SMTP_USERNAME"
  smtpPassword <- reqEnv "SMTP_PASSWORD"

  universe <- flip runSqlPool getPool $ do
    doMigrations
    checkBooleans
    mkUniverse
  getUVar <- newMVar universe

  let conf   = Config {..}
      logger = mkLogger getEnv
      corsPolicy = simpleCorsResourcePolicy
        { corsMethods = simpleMethods ++ ["PUT", "DELETE"]
        , corsRequestHeaders = simpleHeaders ++ ["Authorization"]
        }

  putConf conf

  putStrLn $ "Now running on port " ++ show getPort
  run getPort $ logger $ cors (const $ Just corsPolicy) $ mkApp conf
