module Presenter.Theorem
( theoremName
, theoremNameLinked
, formulaNameLinked
, theoremTitle
) where

import Import

import Data.List (foldl1)
import qualified Data.Map as M
import qualified Data.Text as T

import DB (Prefetch)
import Models
import Presenter.Property (propertyNameAtom)


-- TODO: the name / linked name functions should be Text / Html, not Widgets
--         to enforce the "no-queries-here" guarantee and stave off n+1s

widgetJoin :: Widget -> [Widget] -> Widget
widgetJoin sep ws = foldl1 (<>) $ intersperse sep ws

andW, orW :: Widget
andW = [whamlet|\ & |]
orW  = [whamlet|\ | |]

enclose :: Widget -> Widget
enclose w = [whamlet|
$newline never
(^{w})|]

linkedAtom :: (Entity Property) -> Bool -> Widget
linkedAtom (Entity _id p) v = [whamlet|
$newline never
<a href=@{PropertyR _id}>#{propertyNameAtom p v}|]

formulaWidget :: (a -> Bool -> Widget) -> Formula a -> Widget
formulaWidget r (And  sf ) = enclose . widgetJoin andW $ map (formulaWidget r) sf
formulaWidget r (Or   sf ) = enclose . widgetJoin  orW $ map (formulaWidget r) sf
formulaWidget r (Atom p v) = r p v

formulaNameLinked :: Formula (Entity Property) -> Widget
formulaNameLinked = formulaWidget linkedAtom

renderTheorem :: Prefetch Property -> (Entity Property -> Bool -> Widget) -> Theorem -> Widget
renderTheorem ps f theorem = do
  let (Implication ant con) = theoremImplication theorem
  let f' _id = f . Entity _id $ (M.!) ps _id
  [whamlet|
$newline never
^{formulaWidget f' ant} ⇒ ^{formulaWidget f' con}|]

theoremName :: Prefetch Property -> Theorem -> Widget
theoremName properties = renderTheorem properties $ \(Entity _ p) v ->
  [whamlet|
$newline never
<span>#{propertyNameAtom p v}|]

theoremNameLinked :: Prefetch Property -> Theorem -> Widget
theoremNameLinked properties = renderTheorem properties linkedAtom

theoremTitle :: Prefetch Property -> Theorem -> Text
theoremTitle ps = T.pack . show . fmap fetchName . theoremImplication
  where
    fetchName = T.unpack . propertyName . (M.!) ps
