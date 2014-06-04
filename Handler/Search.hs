module Handler.Search where

import Import
import qualified Data.Map as M
import qualified Data.Set as S

import Handler.Helpers
import Handler.Partials (linkedFormula)
import Logic (matches)
import Util (decodeText)

searchHelp :: Widget
searchHelp = do
  let s = SpaceR . Key . PersistInt64
  $(widgetFile "search/help")

parseFormula :: Text -> Handler (Maybe (Formula (Entity Property)))
parseFormula _ = do
  mp <- runDB $ selectFirst [] []
  return $ fmap (\p -> Atom p True) mp

formulaLookup :: Formula PropertyId -> Handler (Formula (Entity Property))
formulaLookup f = do
  let pids = S.toList . formulaProperties $ f
  props <- runDB $ selectList [PropertyId <-. pids] []
  let h = M.fromList . map (\p -> (entityKey p, p)) $ props
  return . fmap ((M.!) h) $ f

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
        Just f'' -> do
          let f' = fmap (Key . PersistInt64) f''
          spaceIds <- matches Yes f'
          let total = S.size spaceIds
          (spaces, pageWidget) <- paged 10 [SpaceId <-. (S.toList spaceIds)] [Asc SpaceName]
          f <- formulaLookup f'
          render "Search" $(widgetFile "search/results")
