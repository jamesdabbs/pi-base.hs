{-# LANGUAGE OverloadedStrings #-}

import Base

import Database.Persist
import Database.Persist.Sql        (rawExecute)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import Network.Wai                 (Application)
import Test.Hspec
import Test.Hspec.Wai

import Control.Concurrent.STM.TVar (newTVarIO)

import Config (mkPool)
import Api    (mkApp)
import Models (doMigrations)
import qualified Universe as U

import Spec.App   (appSpecs)
import Spec.Logic (logicSpecs)

app :: IO Application
app = do
  tu   <- newTVarIO $ U.empty
  pool <- mkPool Test

  return $ mkApp $ Config
    { getEnv  = Test
    , getPool = pool
    , getUVar = tu
    }

reset :: ConnectionPool -> IO ()
reset = runSqlPool $ do
  rawExecute "truncate table spaces cascade" []
  _ <- insertUnique $ User "test@example.com" (Just "James") True Nothing Nothing
  return ()

allSpecs :: Spec
allSpecs = do
  logicSpecs

  pool <- runIO $ mkPool Test
  runIO $ runSqlPool doMigrations pool

  before (reset pool) $ do
    it "resets between specs" $ do
      n <- flip runSqlPool pool $ count ([] :: [Filter Space])
      n `shouldBe` (0 :: Int)

    with app $ appSpecs

main :: IO ()
main = hspec allSpecs
