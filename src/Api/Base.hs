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

import Control.Monad.Catch (catchAll, SomeException)
import Data.Aeson (encode, object, (.=))
import Servant as Api.Base hiding (serve)

runAction :: Config -> Action v -> EitherT ServantErr IO v
runAction conf action = runReaderT (unAction action) conf

unsafeRunAction :: Action v -> IO (Either ServantErr v)
unsafeRunAction action = runEitherT $ do
  conf <- lift $ getConf
  runAction conf action

errorHandler :: Config -> HandlerContext -> SomeException -> EitherT ServantErr IO ()
errorHandler conf ctx err = liftIO $ putStrLn "should send to rollbar"

runHandler :: Config -> HandlerContext -> Handler a -> EitherT ServantErr IO a
runHandler conf ctx h = catchAll go alert
  where
    go = runAction conf $ evalStateT (unHandler h) ctx
    alert err = do
      errorHandler conf ctx err
      left err500 { errBody = encode $ object ["error" .= show err] } -- TODO: unify

actionToHandler :: Action a -> Handler a
actionToHandler = Handler . lift

halt :: ServantErr -> Action a
halt err = Action . lift . left $ err'
  where
    err' = err { errHeaders = [("Content-Type", "application/json")] }
