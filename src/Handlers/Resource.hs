{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
module Handlers.Resource
  ( Resource
  , ResourceHandler
  , resource
  ) where

import Base

import Database.Persist
import Database.Persist.Sql
import Servant

import Api.Combinators
import Handlers.Helpers
import Models (runDB)

type Resource a =
       Get '[JSON] [Entity a]
  :<|> ReqBody '[JSON] a :> Authenticated :> Post '[JSON] (Entity a)
  :<|> Capture "id" (Key a) :>
         ( Get '[JSON] (Entity a)
         :<|> ReqBody '[JSON] a :> Authenticated :> Put '[JSON] (Entity a)
         :<|> Authenticated :> Delete '[] ()
         )

type ResourceHandler a = Action [Entity a]
                       :<|> ( a -> AuthenticatedAction (Entity a) )
                       :<|> ( Key a ->
                         ( Action (Entity a)
                         :<|> (a -> AuthenticatedAction (Entity a))
                         :<|> (AuthenticatedAction ())
                         ) )

resource :: (PersistEntity a, PersistEntityBackend a ~ SqlBackend) => Proxy a -> ResourceHandler a
resource _ = index :<|> create :<|> instances
  where
    index = runDB $ selectList [] []

    create obj = withUser $ \_ -> do
      _id <- runDB $ insert obj
      return $ Entity _id obj

    instances _id = get404 _id :<|> put _id :<|> del _id

    put _id obj = withUser $ \_ -> do
      _ <- runDB $ replace _id $ obj
      return $ Entity _id obj

    del _id = withUser $ \_ -> do
      runDB $ delete _id
