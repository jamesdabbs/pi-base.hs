module Handler.Helpers
( paged
, preview
, plural
, render
, authButton
, flash
, FlashClass (..)
) where

import Import

import qualified Data.Text as T

import Database.Persist.Sql (SqlBackend)
import Yesod.Paginator hiding (paginate)
import Yesod.Routes.Class (Route)

render :: Text -> Widget -> Handler Html
render title w = defaultLayout $ do
  setTitle . toHtml $ [shamlet|#{title} | 𝜋-Base|]
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

plural :: Int -> Text -> Text -> Text
plural 1 _ x = "1 " <> x
plural n x _ = (T.pack $ show n) <> " " <> x

-- TODO: why doesn't this type signature seem to work?
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

-- TODO: is there any way to get the "you are now logged in" message styled?
flash :: FlashClass -> Text -> Handler ()
flash c msg = setMessage [shamlet|<.alert.alert-#{show c}>#{msg}|]
