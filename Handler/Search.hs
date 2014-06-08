module Handler.Search where

import Import
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Handler.Helpers
import Handler.Partials (linkedFormula)
import Logic (matches)
import Util (decodeText)

searchHelp :: Widget
searchHelp = do
  let s = SpaceR . Key . PersistInt64
  $(widgetFile "search/help")

formulaLookup :: Formula PropertyId -> Handler (Formula (Entity Property))
formulaLookup f = do
  let pids = S.toList . formulaProperties $ f
  props <- runDB $ selectList [PropertyId <-. pids] []
  let h = M.fromList . map (\p -> (entityKey p, p)) $ props
  return . fmap ((M.!) h) $ f

searchShow :: Maybe Text -> Widget
searchShow q = $(widgetFile "search/show")

searchByText :: Text -> Handler Html
searchByText = undefined

matchTypeDisplay :: MatchType -> Text
matchTypeDisplay Yes = "∋"
matchTypeDisplay No = "∌"
matchTypeDisplay Unknown = "?"

searchByFormula :: Text -> MatchType -> Text -> Handler Html
searchByFormula qt _type text = do
  let mf = decodeText text
  case mf of
    Nothing -> do
      render "Search" $(widgetFile "search/malformed")
    Just f'' -> do
      let f' = fmap (Key . PersistInt64) f''
      spaceIds <- matches _type f'
      let total = S.size spaceIds
      (spaces, pager) <- paged 10 [SpaceId <-. (S.toList spaceIds)] [Asc SpaceName]
      f <- formulaLookup f'
      render "Search" $(widgetFile "search/results")

getSearchR :: Handler Html
getSearchR = do
  q <- lookupGetParam "q"
  case q of
    Nothing -> render "Search" (searchShow q)
    Just qt -> do
      case T.uncons qt of
        Just (':', t) -> searchByText t
        Just ('?', t) -> searchByFormula qt Unknown t
        Just ('!', t) -> searchByFormula qt No t
        Just _ -> searchByFormula qt Yes qt
        Nothing -> render "Search" (searchShow q)

