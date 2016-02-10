{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Concurrent.STM.TVar (newTVarIO)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai.Handler.Warp
-- import Servant
-- import Servant.JQuery
import System.Environment (lookupEnv)

import Api    (mkApp)
import Config (mkPool, mkLogger)
import Models (doMigrations, checkBooleans, mkUniverse)
import Types

env :: Read a => String -> a -> IO a
env k def = do
  mv <- lookupEnv k
  return $ case mv of
    Nothing -> def
    Just  v -> read v

main :: IO ()
main = do
  -- writeFile "public/js/api.js" $ jsForAPI (Proxy :: Proxy API)

  mode <- env "ENV" Development
  port <- env "PORT" 8081
  pool <- mkPool mode

  universe <- flip runSqlPool pool $ do
    doMigrations
    checkBooleans
    mkUniverse
  tu <- newTVarIO universe

  let conf   = Config { getPool = pool, getEnv = mode, getUVar = tu }
      logger = mkLogger mode

  putStrLn $ "Now running on port " ++ show port
  run port $ logger $ mkApp conf
