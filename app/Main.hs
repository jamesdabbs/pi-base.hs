module Main where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader (runReaderT)
import Database.Persist.Postgresql (runSqlPool)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment (lookupEnv)

import Api
import Config (mkPool, mkLogger)
import Models (doMigrations)
import Types


env :: Read a => String -> a -> IO a
env k def = do
  mv <- lookupEnv k
  return $ case mv of
    Nothing -> def
    Just  v -> read v

mkApp :: Config -> Application
mkApp conf = serve api $ enter (Nat run) handlers
  where
    api :: Proxy API
    api = Proxy

    run :: Action v -> EitherT ServantErr IO v
    run a = runReaderT (runAction a) conf

main :: IO ()
main = do
  mode <- env "ENV" Development
  port <- env "PORT" 8081
  pool <- mkPool mode

  let conf = Config { getPool = pool, getEnv = mode }
      log  = mkLogger mode

  runSqlPool doMigrations pool

  putStrLn $ "Now running on port " ++ show port
  run port $ log $ mkApp conf
