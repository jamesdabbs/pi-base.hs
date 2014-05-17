module Handler.Partials
( traitName
, linkedTraitName
, theoremName
) where

import Import

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

theoremName :: Theorem -> Widget
theoremName theorem = do
  toWidget [whamlet|<span>#{show $ theoremAntecedent theorem} => #{show $ theoremConsequent theorem}|]
