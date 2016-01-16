module Main where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader (runReaderT)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)

import Api
import Config (mkPool, mkLogger)
import Models (doMigrations, mkUniverse)
import Types


env :: Read a => String -> a -> IO a
env k def = do
  mv <- lookupEnv k
  return $ case mv of
    Nothing -> def
    Just  v -> read v

mkApp :: Config -> Application
mkApp conf = serve api $ enter (Nat runner) handlers
  where
    api :: Proxy API
    api = Proxy

    runner :: Action v -> EitherT ServantErr IO v
    runner a = runReaderT (runAction a) conf

main :: IO ()
main = do
  mode <- env "ENV" Development
  port <- env "PORT" 8081
  pool <- mkPool mode

  -- TODO: better logging for this part
  runSqlPool doMigrations pool
  universe <- runSqlPool mkUniverse pool
  tu <- newTVarIO universe

  let conf   = Config { getPool = pool, getEnv = mode, getTU = tu }
      logger = mkLogger mode

  putStrLn $ "Now running on port " ++ show port
  run port $ logger $ mkApp conf
