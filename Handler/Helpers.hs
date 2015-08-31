module Handler.Helpers
( paged
, preview
, plural
, render
, authButton
, flash
, FlashClass (..)
) where

import Import hiding (toLower)

import Data.Char (toLower)
import qualified Data.Text as T

import Yesod.Paginator hiding (paginate)

render :: Text -> Widget -> Handler Html
render title w = defaultLayout $ do
  setTitle . toHtml $ [shamlet|#{title} | π-Base|]
  w

paginationConfig :: PageWidget App
paginationConfig = paginationWidget $ PageWidgetConfig
  { prevText     = "«"
  , nextText     = "»"
  , pageCount    = 2
  , ascending    = True
  , showEllipsis = True
  , listClasses  = ["pagination", "pagination-centered"]
  }

plural :: Int -> Text -> Text
plural 1 x = "1 " <> x
-- This is clearly "wrong", but works for all the cases we need
plural n x = (T.pack $ show n) <> " " <> x <> "s"

paged :: (PersistEntity e, PersistEntityBackend e ~ SqlBackend) => Int -> [Filter e]  -> [SelectOpt e] -> Handler ([Entity e], Widget)
paged size q f = runDB $ selectPaginatedWith paginationConfig size q f

preview :: Textarea -> Text
preview t = case T.lines . unTextarea $ t of
  (l:_) -> l
  _     -> ""

authButton :: Route App -> Text -> Widget
authButton route label = do
  can <- handlerToWidget $ isAuthorized route False
  [whamlet|
$if can == Authorized
  <a.btn.btn-default href=@{route}>#{label}
|]

data FlashClass = Success | Info | Warning | Danger deriving (Show)

flash :: FlashClass -> Text -> Handler ()
flash c msg = setMessage [shamlet|<.alert.alert-#{map toLower $ show c}>#{msg}|]
