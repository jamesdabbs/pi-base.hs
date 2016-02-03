{-# LANGUAGE OverloadedStrings #-}

import Base

import Database.Persist
import Database.Persist.Sql        (rawExecute)
import Database.Persist.Postgresql (ConnectionPool, runSqlPool)
import Network.Wai                 (Application)
import Test.Hspec
import Test.Hspec.Wai

import Control.Concurrent.STM.TVar (newTVarIO)
import qualified Data.Map as M

import Config (mkPool)
import Api    (mkApp)
import Models (doMigrations)

import Spec.App (appSpecs)

app :: IO Application
app = do
  tu   <- newTVarIO $ Universe { uspaces = M.empty }
  pool <- mkPool Test

  return $ mkApp $ Config
    { getEnv  = Test
    , getPool = pool
    , getTU   = tu
    }

reset :: ConnectionPool -> IO ()
reset = runSqlPool $ do
  rawExecute "truncate table spaces cascade" []
  _ <- insertUnique $ User "test@example.com" (Just "James") True Nothing Nothing
  return ()

spec :: Spec
spec = do
  pool <- runIO $ mkPool Test
  runIO $ runSqlPool doMigrations pool

  before (reset pool) $ do
    it "resets between specs" $ do
      n <- flip runSqlPool pool $ count ([] :: [Filter Space])
      n `shouldBe` (0 :: Int)

    with app $ appSpecs

main :: IO ()
main = hspec spec
