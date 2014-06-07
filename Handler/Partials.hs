-- TODO: modularize
module Handler.Partials
( traitName
, linkedTraitName
, theoremName
, linkedTheoremName
, linkedFormula
, filteredTraits
, revisionList
, linkedTraitList
) where

import Import

import Data.List (find, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S

import Models
import Handler.Helpers (paged, preview)


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

enclose :: Widget -> Widget
enclose w = [whamlet|
$newline never
(^{w})|]

formulaWidget :: (a -> Bool -> Widget) -> Formula a -> Widget
formulaWidget r (And  sf ) = enclose . widgetJoin andW $ map (formulaWidget r) sf
formulaWidget r (Or   sf ) = enclose . widgetJoin  orW $ map (formulaWidget r) sf
formulaWidget r (Atom p v) = r p v

renderTheorem :: (Entity Property -> Bool -> Widget) -> Theorem -> Widget
renderTheorem f theorem = do
  let i@(Implication ant cons) = theoremImplication theorem
  props <- handlerToWidget . runDB $ selectList [PropertyId <-. (S.toList $ implicationProperties i)] []
  let _lookup = M.fromList . map (\p -> (entityKey p, p)) $ props
  let f' = f . (M.!) _lookup
  [whamlet|
$newline never
^{formulaWidget f' ant} ⇒ ^{formulaWidget f' cons}|]

atomName :: Property -> Bool -> Text
atomName p True = propertyName p
atomName p False = "¬" <> (propertyName p)

theoremName :: Theorem -> Widget
theoremName = renderTheorem $ \(Entity _ p) v ->
  [whamlet|
$newline never
<span>#{atomName p v}|]

linkedAtom :: (Entity Property) -> Bool -> Widget
linkedAtom (Entity _id p) v = [whamlet|
$newline never
<a href=@{PropertyR _id}>#{atomName p v}|]

linkedTheoremName :: Theorem -> Widget
linkedTheoremName = renderTheorem linkedAtom

linkedFormula :: Formula (Entity Property) -> Widget
linkedFormula = formulaWidget linkedAtom

data TraitTab = TraitTab { ttParam :: Text, ttLabel :: Text, ttFilter :: [Filter Trait] }

instance Eq TraitTab where
  a == b = ttParam a == ttParam b

asserted, deduced, unproven :: TraitTab
asserted = TraitTab
  { ttParam = "asserted", ttLabel = "Asserted", ttFilter = [TraitDeduced ==. False] }
deduced  = TraitTab
  { ttParam = "deduced",  ttLabel = "Deduced",  ttFilter = [TraitDeduced ==. True] }
unproven = TraitTab
  { ttParam = "unproven", ttLabel = "Needing Proof"
  , ttFilter = [TraitDeduced ==. False, TraitDescription ==. Textarea ""] }

tabFromParam :: Maybe Text -> TraitTab
tabFromParam p = case find match tabs of
  Just t  -> t
  Nothing -> asserted
  where
    tabs = [asserted, deduced, unproven]
    match tab = p == (Just . ttParam $ tab)

withTTFilter :: TraitTab -> [Filter Trait] -> [Filter Trait]
withTTFilter tab fs = fs ++ (ttFilter tab)

traitTab :: [Filter Trait] -> TraitTab -> TraitTab -> Widget
traitTab fs selected t = do
  n <- handlerToWidget . runDB . count $ withTTFilter t fs
  let klass = if t == selected then ("active" :: Text) else ""
  if n > 0
    then [whamlet|<a class="btn btn-default #{klass}" href="?traits=#{ttParam t}">#{ttLabel t} <span class="badge">#{n}</span>|]
    else [whamlet||]

-- IDEA:
-- class Loadable m where
--   type Loaded = Loaded m
--   load :: [m] -> Handler [Loaded m]
type ELTrait = (Entity Space, Entity Trait, Entity Property)

getEntity _id = do
  v <- runDB . get404 $ _id
  return $ Entity _id v

fetchOne et@(Entity _ t) = do
  s <- getEntity $ traitSpaceId t
  p <- getEntity $ traitPropertyId t
  return (s,et,p)

-- FIXME: n+1
prefetchTraits :: [Entity Trait] -> Handler [ELTrait]
prefetchTraits = mapM fetchOne

filteredTraits :: (ELTrait -> Text) -> [Filter Trait] -> Widget
filteredTraits renderer fs = do
  param <- lookupGetParam "traits"
  let tab = tabFromParam param
  (traits, pageWidget) <- handlerToWidget $ paged 10 (withTTFilter tab fs) []
  traitTuples <- handlerToWidget $ prefetchTraits traits
  $(widgetFile "traits/_filtered")

revisionList :: (Revisable a) => Entity a -> Widget
revisionList e = do
  revs <- handlerToWidget . revisions $ e
  $(widgetFile "revisions/_list")

linkedTraitList :: [Entity Trait] -> Widget
linkedTraitList traits = $(widgetFile "traits/_linked_list")
