module Handler.Helpers
( page
, preview
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

-- TODO: type signature
page size = runDB $ selectPaginatedWith paginationConfig (size::Int) [] []

preview :: Text -> Text
preview t = case T.lines t of
  (l:_) -> l
  _     -> ""
