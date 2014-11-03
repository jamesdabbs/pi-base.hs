module Handler.Search where

import Import
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Database.Persist.Sql

import DB (icontains)
import Handler.Helpers
import Logic (matches)
import Presenter.Theorem (formulaNameLinked)
import Util (decodeText)

searchHelp = error "searchHelp | Handler/Search"

formulaLookup :: Formula PropertyId -> Handler (Formula (Entity Property))
formulaLookup f = do
  let pids = S.toList . formulaProperties $ f
  props <- runDB $ selectList [PropertyId <-. pids] []
  let h = M.fromList . map (\p -> (entityKey p, p)) $ props
  return . fmap ((M.!) h) $ f

searchShow :: Maybe Text -> Widget
searchShow q = error "searchShow" -- $(widgetFile "search/show")

searchByText :: Text -> Text -> Handler Html
searchByText qt text = error "searchByText"
  -- spaces     <- runDB $ count [icontains SpaceName text]
  -- properties <- runDB $ count [icontains PropertyName text]
  -- _type <- lookupGetParam "type"
  -- -- TODO: extract this and filtered trait tab widget
  -- case _type of
  --   Just "properties" -> do
  --     (results, pager) <- paged 10 [icontains PropertyName text] []
  --     render (qt <> " properties") $(widgetFile "search/property_text_results")
  --   _ -> do
  --     (results, pager) <- paged 10 [icontains SpaceName text] []
  --     render (qt <> " spaces") $(widgetFile "search/space_text_results")

matchTypeDisplay :: MatchType -> Text
matchTypeDisplay Yes = "∋"
matchTypeDisplay No = "∌"
matchTypeDisplay Unknown = "?"

searchByFormula :: Text -> MatchType -> Text -> Handler Html
searchByFormula qt _type text = error "searchByFormula"
  -- let mf = decodeText text
  -- case mf of
  --   Nothing -> do
  --     render "Search" $(widgetFile "search/malformed")
  --   Just f'' -> do
  --     let f' = fmap (PropertyKey . SqlBackendKey) f''
  --     spaceIds <- matches _type f'
  --     let total = S.size spaceIds
  --     (spaces, pager) <- paged 10 [SpaceId <-. (S.toList spaceIds)] [Asc SpaceName]
  --     f <- formulaLookup f'
  --     render "Search" $(widgetFile "search/results")

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

getContributeR :: Handler Html
getContributeR = do
  let q = "SELECT ?? FROM traits WHERE description='' AND deduced=False ORDER BY random() LIMIT 1"
  result <- runDB $ rawSql q []
  case result of
    (Entity _id _) : _ -> do
      flash Info "We need your help! Can you supply a proof of this assertion?"
      redirect $ TraitR _id
    _ -> do
      error "Every assertion has a proof. That's awesome, but this endpoint has outlived its usefulness"
