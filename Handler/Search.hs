module Handler.Search where

import Import
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Persist.Sql (rawSql)

import Handler.Helpers
import Handler.Partials (linkedFormula)
import Logic (matches)
import Util (decodeText)

searchHelp :: Widget
searchHelp = do
  let s = SpaceR . Key . PersistInt64
  $(widgetFile "search/help")

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "𝜋-Base"
  $(widgetFile "homepage")

formulaLookup :: Formula PropertyId -> Handler (Formula (Entity Property))
formulaLookup f = do
  let pids = S.toList . formulaProperties $ f
  props <- runDB $ selectList [PropertyId <-. pids] []
  let h = M.fromList . map (\p -> (entityKey p, p)) $ props
  return . fmap ((M.!) h) $ f

searchShow :: Maybe Text -> Widget
searchShow q = $(widgetFile "search/show")

searchByText :: Text -> Text -> Handler Html
searchByText qt text = do
  let q = "SELECT ?? FROM spaces WHERE spaces.name ILIKE ?"
  spaces <- runDB $ rawSql q [PersistText $ "%" <> text <> "%"]
  render "Search" $(widgetFile "search/text_results")

matchTypeDisplay :: MatchType -> Text
matchTypeDisplay Yes = "∋"
matchTypeDisplay No = "∌"
matchTypeDisplay Unknown = "?"

-- TODO: search across spaces, properties, descriptions, aliases ... probably want ES for that
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
        Just (':', t) -> searchByText qt t
        Just ('?', t) -> searchByFormula qt Unknown t
        Just ('!', t) -> searchByFormula qt No t
        Just _ -> searchByFormula qt Yes qt
        Nothing -> render "Search" (searchShow q)

