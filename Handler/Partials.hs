module Handler.Partials
( filteredTraits
, revisionList
, searchHelp
) where

import Import

import Data.List (find)

import DB (Prefetch)
import Handler.Helpers (paged, preview)
import Models


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

filteredTraits :: (Prefetch Space -> Prefetch Property -> Trait -> Widget) -> [Filter Trait] -> Widget
filteredTraits renderer fs = do
  param <- lookupGetParam "traits"
  let tab = tabFromParam param
  (traits, pager) <- handlerToWidget $ paged 10 (withTTFilter tab fs) [Desc TraitId]
  (spaces, properties) <- handlerToWidget $ traitPrefetch traits
  $(widgetFile "traits/_filtered")

revisionList :: (Revisable a) => Entity a -> Widget
revisionList e = do
  revs <- handlerToWidget . revisions $ e
  $(widgetFile "revisions/_list")

searchHelp :: Widget
searchHelp = do
  let s = SpaceR . Key . PersistInt64
  $(widgetFile "search/help")
