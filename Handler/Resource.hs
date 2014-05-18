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

-- TODO: type signature
page size = runDB $ selectPaginatedWith paginationConfig (size::Int) [] []
