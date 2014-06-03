module Handler.Partials
( traitName
, linkedTraitName
, theoremName
, linkedTheoremName
, linkedFormula
, filteredTraits
, revisionList
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
  [whamlet|<span>#{spaceName s}: #{propertyName p}=#{tValueName v}|]

linkedTraitName :: Trait -> Widget
linkedTraitName trait = do
  (space, property, value) <- handlerToWidget . traitTuple $ trait
  $(widgetFile "traits/linked_name")

widgetJoin :: Widget -> [Widget] -> Widget
widgetJoin sep ws = foldl1 (<>) $ intersperse sep ws

andW, orW :: Widget
andW = [whamlet|\ & |]
orW  = [whamlet|\ | |]

-- FIXME: strip trailing space
enclose :: Widget -> Widget
enclose w = [whamlet|( ^{w})|]

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
  [whamlet|^{formulaWidget f' ant} ⇒ ^{formulaWidget f' cons}|]

atomName :: Property -> Bool -> Text
atomName p True = propertyName p
atomName p False = "¬" <> (propertyName p)

theoremName :: Theorem -> Widget
theoremName = renderTheorem $ \(Entity _ p) v ->
  [whamlet|<span>#{atomName p v}|]

linkedAtom :: (Entity Property) -> Bool -> Widget
linkedAtom (Entity _id p) v = [whamlet|<a href=@{PropertyR _id}>#{atomName p v}|]

linkedTheoremName :: Theorem -> Widget
linkedTheoremName = renderTheorem linkedAtom

-- TODO: factor this and `renderTheorem` together
linkedFormula :: Formula PropertyId -> Widget
linkedFormula f = do
  let fps = S.toList $ formulaProperties f
  props <- handlerToWidget . runDB $ selectList [PropertyId <-. fps] []
  let _lookup = M.fromList . map (\p -> (entityKey p, p)) $ props
  let linkedAtom' = linkedAtom . (M.!) _lookup
  [whamlet|^{formulaWidget linkedAtom' f}|]

paramToFilter :: Maybe Text -> [Filter Trait]
paramToFilter param = case param of
    (Just "deduced")  -> [TraitDeduced ==. True]
    (Just "unproven") -> [TraitDeduced ==. False, TraitDescription ==. Textarea ""]
    _                 -> [TraitDeduced ==. False]

filteredTraits :: [Filter Trait] -> Widget
filteredTraits fs = do
  param <- lookupGetParam "traits"
  (traits, pageWidget) <- handlerToWidget $ paged 10 (paramToFilter param ++ fs) []
  $(widgetFile "traits/_filtered")

revisionList :: (Revisable a) => Entity a -> Widget
revisionList e = do
  revs <- handlerToWidget . revisions $ e
  $(widgetFile "revisions/_list")
