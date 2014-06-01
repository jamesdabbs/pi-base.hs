module Handler.Partials
( traitName
, linkedTraitName
, theoremName
, linkedTheoremName
, filteredTraits
) where

import Import

import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Set as S

import Models
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

widgetJoin :: Widget -> [Widget] -> Widget
widgetJoin sep ws = foldl1 (<>) $ intersperse sep ws

andW, orW :: Widget
andW = toWidget [whamlet|\ & |]
orW  = toWidget [whamlet|\ | |]

-- FIXME: strip trailing space
enclose :: Widget -> Widget
enclose w = toWidget [whamlet|( ^{w})|]

formulaWidget :: (PropertyId -> Bool -> Widget) -> Formula PropertyId -> Widget
formulaWidget r (And  sf ) = enclose . widgetJoin andW $ map (formulaWidget r) sf
formulaWidget r (Or   sf ) = enclose . widgetJoin  orW $ map (formulaWidget r) sf
formulaWidget r (Atom p v) = r p v

renderTheorem :: (Entity Property -> Bool -> Widget) -> Theorem -> Widget
renderTheorem f theorem = do
  let i@(Implication ant cons) = theoremImplication theorem
  props <- handlerToWidget . runDB $ selectList [PropertyId <-. (S.toList $ implicationProperties i)] []
  let _lookup = M.fromList . map (\p -> (entityKey p, p)) $ props
  let f' = f . (M.!) _lookup
  toWidget [whamlet|^{formulaWidget f' ant} ⇒ ^{formulaWidget f' cons}|]

atomName :: Property -> Bool -> Text
atomName p True = propertyName p
atomName p False = "¬" <> (propertyName p)

theoremName :: Theorem -> Widget
theoremName = renderTheorem $ \(Entity _ p) v ->
  toWidget [whamlet|<span>#{atomName p v}|]

linkedTheoremName :: Theorem -> Widget
linkedTheoremName = renderTheorem $ \(Entity pid p) v ->
  toWidget [whamlet|<a href=@{PropertyR pid}>#{atomName p v}|]

paramToFilter :: Maybe Text -> [Filter Trait]
paramToFilter param = case param of
    (Just "deduced")  -> [TraitDeduced ==. True]
    (Just "unproven") -> [TraitDeduced ==. False, TraitDescription ==. ""]
    _                 -> [TraitDeduced ==. False]

filteredTraits :: [Filter Trait] -> Widget
filteredTraits fs = do
  param <- lookupGetParam "traits"
  (traits, pageWidget) <- handlerToWidget $ paged (paramToFilter param ++ fs) 10
  total <- handlerToWidget . runDB $ count ([] :: [Filter Trait])
  $(widgetFile "traits/_filtered")
