module Presenter.Trait
( traitName
, traitNameAtom
, traitNameLinked
, traitListLinked
) where

import Import

import qualified Data.Map as M

import DB (Prefetch)
import Model.Trait
import Presenter.Property (propertyNameAtom)

traitName :: Prefetch Space -> Prefetch Property -> Trait -> Widget
traitName ss ps trait = [whamlet|#{spaceName s}: ^{traitNameAtom ss ps trait}|]
  where
    s = (M.!) ss $ traitSpaceId trait

traitNameAtom :: Prefetch Space -> Prefetch Property -> Trait -> Widget
traitNameAtom _ ps trait = [whamlet|#{propertyNameAtom p v}|]
  where
    p = (M.!) ps $ traitPropertyId trait
    v = traitValueBool trait

traitNameLinked :: Prefetch Space -> Prefetch Property -> Trait -> Widget
traitNameLinked ss ps trait = do
  let property = (M.!) ps $ traitPropertyId trait
  let space = (M.!) ss $ traitSpaceId trait
  $(widgetFile "traits/linked_name")

traitListLinked :: Prefetch Space -> Prefetch Property -> [Entity Trait] -> Widget
traitListLinked spaces properties traits = do
  [whamlet|
<ul>
  $forall (Entity _id t) <- traits
    <li>
      <a href=@{TraitR _id}> ^{traitName spaces properties t}|]
