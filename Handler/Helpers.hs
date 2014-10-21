module Handler.Helpers
( paged
, paged'
, preview
, plural
, render
, authButton
, flash
, FlashClass (..)
) where

import Import

import Prelude (head)
import Data.Char (toLower)
import qualified Data.Text as T

import Database.Persist.Sql (SqlBackend)
--import Yesod.Paginator hiding (paginate)
--import Yesod.Routes.Class (Route)

render :: Text -> Widget -> Handler Html
render title w = defaultLayout $ do
  setTitle . toHtml $ [shamlet|#{title} | π-Base|]
  w

--paginationConfig :: PageWidget App
--paginationConfig = paginationWidget $ PageWidgetConfig
--  { prevText     = "«"
--  , nextText     = "»"
--  , pageCount    = 2
--  , ascending    = True
--  , showEllipsis = True
--  , listClasses  = ["pagination", "pagination-centered"]
--  }

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
