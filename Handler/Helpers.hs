module Handler.Helpers
( page
, paged
, preview
, plural
) where

import Import

import qualified Data.Text as T

import Yesod.Paginator hiding (paginate)

-- TODO: this module is becoming Handler.Helpers

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
plural 1 _ x = x
plural _ x _ = x

-- TODO: why doesn't this type signature seem to work?
paged q size = runDB $ selectPaginatedWith paginationConfig size q []
page = paged []

preview :: Text -> Text
preview t = case T.lines t of
  (l:_) -> l
  _     -> ""
