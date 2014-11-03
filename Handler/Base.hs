module Handler.Base
( index
, index'
, create
, show
, update
, delete
, revisions
, paged
) where

import qualified Prelude as P (head, show)
import Import hiding (show, delete, update)

import qualified Data.Text as T
import Database.Persist.Sql (SqlBackend)

import Handler.Helpers (requireAdmin, requireUser)
import Form (runJsonForm)
import qualified Model.Revision as R


coerceInt :: Text -> Int -> Int
coerceInt from fallback = do
  let parsed = reads . T.unpack $ from
  if length parsed == 1
    then fst . P.head $ parsed
    else fallback

getIntParam :: Text -> Int -> Handler Int
getIntParam name fallback = do
  val <- lookupGetParam name
  return $ case val of
    Just n  -> coerceInt n fallback
    Nothing -> fallback

paged :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => [Filter e] -> [SelectOpt e] -> Handler [Entity e]
paged filters options = do
  total <- runDB $ count filters
  addHeader "X-Total-Count" (T.pack $ P.show total)

  page    <- getIntParam "page" 1
  perPage <- getIntParam "perPage" 50
  let limit  = min perPage 50
  let offset = (page - 1) * limit
  results <- runDB $ selectList filters ([LimitTo limit, OffsetBy offset] ++ options)
  return results


index' fs order presenter = paged fs order >>= returnJson . map presenter
index = index' []

create form creator presenter = do
  _ <- requireUser
  d <- runJsonForm form
  o <- creator d
  returnJson . presenter $ o

show presenter = (runDB . get404) >=> returnJson . presenter

update form updater presenter _id = do
  _ <- requireAdmin
  d <- runJsonForm form
  o <- runDB $ get404 _id
  let e = Entity _id o
  u <- updater e d
  returnJson . presenter $ u

delete destructor presenter _id = do
  _ <- requireAdmin
  o <- runDB $ get404 _id
  _ <- destructor _id
  returnJson . presenter $ Entity _id o

revisions _id = do
  o <- runDB $ get404 _id
  r <- R.revisions $ Entity _id o
  returnJson r
