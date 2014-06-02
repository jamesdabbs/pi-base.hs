module Handler.Helpers
( paged
, preview
, plural
, render
) where

import Import

import qualified Data.Text as T

import Yesod.Paginator hiding (paginate)

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
paged size q f = runDB $ selectPaginatedWith paginationConfig size q f

preview :: Textarea -> Text
preview t = case T.lines . unTextarea $ t of
  (l:_) -> l
  _     -> ""
