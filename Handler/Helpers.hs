module Handler.Helpers
( paged
, paged'
, preview
, plural
, render
, requireUser
, requireAdmin
, requireGetParam
, authButton
, flash
, FlashClass (..)
) where

import Import

import Prelude (head)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types

import Database.Persist.Sql (SqlBackend)

render :: Text -> Widget -> Handler Html
render title w = defaultLayout $ do
  setTitle . toHtml $ [shamlet|#{title} | π-Base|]
  w


plural :: Int -> Text -> Text
plural 1 x = "1 " <> x
plural n x = (T.pack $ show n) <> " " <> x

coerceInt :: Text -> Int -> Int
coerceInt from fallback = do
  let parsed = reads . T.unpack $ from
  if length parsed == 1
    then fst . head $ parsed
    else fallback

getIntParam :: Text -> Int -> Handler Int
getIntParam name fallback = do
  val <- lookupGetParam name
  return $ case val of
    Just n  -> coerceInt n fallback
    Nothing -> fallback

paged :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => Int -> [Filter e]  -> [SelectOpt e] -> Handler ([Entity e], Widget)
paged = undefined
--paged size q f = runDB $ selectPaginatedWith paginationConfig size q f

-- WARNING: this _sets_ X-Total-Count, and so may not work as expected if called multiple times in one handler
paged' :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => [Filter e] -> [SelectOpt e] -> Handler [Entity e]
paged' filters options = do
  total <- runDB $ count filters
  addHeader "X-Total-Count" (T.pack $ show total)

  page    <- getIntParam "page" 1
  perPage <- getIntParam "perPage" 50
  let limit  = min perPage 50
  let offset = (page - 1) * limit
  results <- runDB $ selectList filters ([LimitTo limit, OffsetBy offset] ++ options)
  return results

preview :: Textarea -> Text
preview t = case T.lines . unTextarea $ t of
  (l:_) -> l
  _     -> ""

--authButton :: Route App -> Text -> Widget
authButton route label = do
  let label2 = label :: Text -- FIXME
  can <- handlerToWidget $ isAuthorized route False
  [whamlet|
$if can == Authorized
  <a.btn.btn-default href=@{route}>#{label2}
|]

data FlashClass = Success | Info | Warning | Danger deriving (Show)

flash :: FlashClass -> Text -> Handler ()
flash c msg = setMessage [shamlet|<.alert.alert-#{map toLower $ show c}>#{msg}|]

sendError :: MonadHandler m => Status -> m a
sendError status = sendResponseStatus status $ object [ "error" .= (decodeUtf8 . statusMessage $ status) ]

requireUser :: Handler (Entity User)
requireUser = do
  let unauthed = sendError unauthorized401
  authHeader <- lookupHeader "Authorization"
  case authHeader of
    Nothing -> unauthed
    -- FIXME! - secure this auth!
    Just token -> do
      muser <- runDB . getBy . UniqueUser . decodeUtf8 $ token
      case muser of
        Nothing -> unauthed
        Just user -> return user

requireAdmin :: Handler (Entity User)
requireAdmin = do
  user <- requireUser
  case userAdmin . entityVal $ user of
    True  -> return user
    False -> sendError forbidden403

invalid422 :: Status
invalid422 = Status 422 "Invalid"

requireGetParam :: MonadHandler m => Text -> m Text
requireGetParam key = do
  mval <- lookupGetParam key
  case mval of
    Nothing  -> sendError invalid422
    Just val -> return val
