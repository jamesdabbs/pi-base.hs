{-# LANGUAGE OverloadedStrings #-}
module Api.Base
  ( module Api.Types
  , module Base
  , module Api.Base
  , pageJSON
  ) where

import Api.Types
import Base
import Config (getConf)
import Pager  (pageJSON)

import Data.Aeson (encode)
import Servant as Api.Base hiding (serve)

runA :: Config -> Action v -> EitherT ServantErr IO v
runA conf action = runReaderT (runAction action) conf

unsafeRunA :: Action v -> IO (Either ServantErr v)
unsafeRunA action = runEitherT $ do
  conf <- lift $ getConf
  runA conf action

halt :: ServantErr -> Action a
halt err = Action . lift . left $ err'
  where
    err' = err
      { errBody    = encode err
      , errHeaders = [("Content-Type", "application/json")]
      }
