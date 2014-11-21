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

import qualified Prelude as P (head)
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

paged :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => [Filter e] -> [SelectOpt e] -> Handler ([Entity e], Int, Int)
paged filters options = do
  page    <- getIntParam "page" 1
  perPage <- getIntParam "perPage" 50
  let limit  = min perPage 50
  let offset = (page - 1) * limit
  results <- runDB $ selectList filters ([LimitTo limit, OffsetBy offset] ++ options)

  total <- runDB $ count filters
  let total_pages = (total - 1) `div` perPage + 1
  return (results, total, total_pages)


index' :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend, ToJSON b) =>
          Text -> [Filter e] -> [SelectOpt e] -> (Entity e -> b) -> Handler Value
index' name fs order presenter = do
  (results, total, total_pages) <- paged fs order
  returnJson $ object
    [ name .= map presenter results
    , "meta" .= object
      [ "total" .= total
      , "total_pages" .= total_pages
      ]
    ]

index :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend, ToJSON b) =>
         Text -> [SelectOpt e] -> (Entity e -> b) -> Handler Value
index name = index' name []

create :: ToJSON presentation =>
          FormInput Handler createData ->
          (createData -> Handler resource) ->
          (resource -> presentation) ->
          Handler Value
create form creator presenter = do
  _ <- requireUser
  d <- runJsonForm form
  o <- creator d
  returnJson . presenter $ o

show :: (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend, ToJSON presentation) =>
        (Entity rec -> presentation) ->
        Key rec ->
        Handler Value
show presenter _id = do
  o <- runDB $ get404 _id
  returnJson . presenter $ Entity _id o

update :: (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend, ToJSON presentation) =>
          FormInput Handler updateData ->
          (Entity rec -> updateData -> Handler resource) ->
          (resource -> presentation) ->
          Key rec ->
          Handler Value
update form updater presenter _id = do
  _ <- requireAdmin
  d <- runJsonForm form
  o <- runDB $ get404 _id
  let e = Entity _id o
  u <- updater e d
  returnJson . presenter $ u

delete :: (PersistEntity rec, PersistEntityBackend rec ~ SqlBackend, ToJSON presentation) =>
          (Key rec -> Handler ()) ->
          (Entity rec -> presentation) ->
          Key rec ->
          Handler Value
delete destructor presenter _id = do
  _ <- requireAdmin
  o <- runDB $ get404 _id
  destructor _id
  returnJson . presenter $ Entity _id o

revisions :: (R.Revisable rec, PersistEntity rec, PersistEntityBackend rec ~ SqlBackend) =>
             Key rec -> Handler Value
revisions _id = do
  o <- runDB $ get404 _id
  r <- R.revisions $ Entity _id o
  returnJson r
