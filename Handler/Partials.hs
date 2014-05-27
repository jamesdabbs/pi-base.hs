module Handler.Partials
( traitName
, linkedTraitName
, theoremName
, linkedTheoremName
, filteredTraits
) where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import DB (theoremImplication)
import Logic.Types (implicationProperties)
import Handler.Helpers (paged)


traitTuple :: Trait -> Handler (Space, Property, TValue)
traitTuple trait = do
  s <- runDB . get404 . traitSpaceId $ trait
  p <- runDB . get404 . traitPropertyId $ trait
  v <- runDB . get404 . traitValueId $ trait
  return (s,p,v)

traitName :: Trait -> Widget
traitName trait = do
  (s,p,v) <- handlerToWidget . traitTuple $ trait
  toWidget [whamlet|<span>#{spaceName s}: #{propertyName p}=#{tValueName v}|]

linkedTraitName :: Trait -> Widget
linkedTraitName trait = do
  (space, property, value) <- handlerToWidget . traitTuple $ trait
  $(widgetFile "traits/linked_name")

renderTheorem :: Show a => (Property -> a) -> Theorem -> Widget
renderTheorem f theorem = do
  let i = theoremImplication theorem
  props <- handlerToWidget . runDB $ selectList [PropertyId <-. (S.toList $ implicationProperties i)] []
  let lookup = M.fromList . map (\(Entity pid p) -> (pid, p)) $ props
  let f' = f . (M.!) lookup
  toWidget [whamlet|<span>#{show $ fmap f' i}|]

theoremName :: Theorem -> Widget
theoremName = renderTheorem propertyName

-- FIXME: figure out how to HTML escape this
linkedTheoremName :: Theorem -> Widget
linkedTheoremName = theoremName

paramToFilter :: Maybe Text -> [Filter Trait]
paramToFilter param = case param of
    (Just "deduced")  -> [TraitDeduced ==. True]
    (Just "unproven") -> [TraitDeduced ==. False, TraitDescription ==. ""]
    _                 -> [TraitDeduced ==. False]

filteredTraits :: [Filter Trait] -> Widget
filteredTraits fs = do
  param <- lookupGetParam "traits"
  (traits, pageWidget) <- handlerToWidget $ paged (paramToFilter param ++ fs) 10
  $(widgetFile "traits/_filtered")
