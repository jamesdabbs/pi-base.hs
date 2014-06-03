module Handler.Search where

import Import
import qualified Data.Set as S

import Handler.Helpers
import Handler.Partials (linkedFormula)
import Logic (matches)
import Util (decodeText)

searchShow :: Maybe Text -> Widget
searchShow q = $(widgetFile "search/show")

getSearchR :: Handler Html
getSearchR = do
  q <- lookupGetParam "q"
  case q of
    Nothing -> render "Search" (searchShow q)
    Just qt -> do
      let mf = decodeText qt
      case mf of
        Nothing -> do
          render "Search" $(widgetFile "search/malformed")
        Just f -> do
          spaceIds <- matches Yes f
          let total = S.size spaceIds
          (spaces, pageWidget) <- paged 10 [SpaceId <-. (S.toList spaceIds)] [Asc SpaceName]
          render "Search" $(widgetFile "search/results")
