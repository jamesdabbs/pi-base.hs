module Handler.Resource
( page
) where

import Import
import Yesod.Paginator hiding (paginate)

paginationConfig :: PageWidget App
paginationConfig = paginationWidget $ PageWidgetConfig
  { prevText     = "«"
  , nextText     = "»"
  , pageCount    = 2
  , ascending    = True
  , showEllipsis = True
  , listClasses  = ["pagination", "pagination-centered"]
  }

page count = runDB $ selectPaginatedWith paginationConfig 10 [] []
