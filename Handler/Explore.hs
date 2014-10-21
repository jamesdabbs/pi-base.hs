module Handler.Explore
( getTraitDataR
) where

import Import
import Yesod.Routes.TH

import qualified Data.Map as M

import DB (derivedTraits, Prefetch)
import Models
import Presenter.Theorem (theoremTitle)
import Presenter.Trait (traitTitle)

type L a = M.Map TraitId [a]
--type R = Route (HandlerSite Handler) -> Text

--renderTrait :: R -> Prefetch Space -> Prefetch Property -> L TheoremId -> L TraitId -> Entity Trait -> Value
renderTrait urlR ss ps struts supports (Entity _id t) = object
  [ "_id" .= _id
  , "name" .= traitTitle ss ps t
  , "description" .= (unTextarea $ traitDescription t)
  , "url" .= (urlR $ TraitR _id)
  , "assumptions" .= object
    [ "theorems" .= M.lookup _id struts
    , "traits" .= M.lookup _id supports
    ]
  ]

--renderTheorem :: R -> Prefetch Property -> Entity Theorem -> Value
renderTheorem urlR ps (Entity _id t) = object
  [ "_id" .= _id
  , "name" .= theoremTitle ps t
  , "description" .= (unTextarea $ theoremDescription t)
  , "url" .= (urlR $ TheoremR _id)
  ]

gather :: L a -> (TraitId, a) -> L a
gather h (_id,v) = M.insert _id nval h
  where
    nval = case M.lookup _id h of
      Just list -> v : list
      Nothing   -> [v]

collapseTuples :: [(TraitId, a)] -> L a
collapseTuples = foldl gather M.empty

-- TODO: clean this up. Maybe redis-cache the result?
--       add a layer for the local proof?
getTraitDataR :: TraitId -> Handler Value
getTraitDataR _id = do
  urlR     <- getUrlRender
  trait    <- runDB $ get404 _id
  derived  <- derivedTraits _id
  supports <- traitSupport _id
  let traits = [Entity _id trait] ++ derived ++ supports
  (spaces, properties) <- traitPrefetch traits
  struts <- runDB $ selectList [StrutTraitId <-. map entityKey traits] []
  let strutLookup = collapseTuples $ map (\(Entity _ s) -> (strutTraitId s, strutTheoremId s)) struts
  supports' <- runDB $ selectList [SupporterImpliedId <-. map entityKey traits] []
  let supportLookup = collapseTuples $ map (\(Entity _ s) -> (supporterImpliedId s, supporterAssumedId s)) supports'
  theorems <- runDB $ selectList [TheoremId <-. map (strutTheoremId . entityVal) struts] []
  tProps <- theoremPrefetch . map entityVal $ theorems
  returnJson $ object
    [ "root" .= map entityKey supports
    , "active" .= _id
    , "derived" .= map entityKey derived
    , "traits" .= map (renderTrait urlR spaces properties strutLookup supportLookup) traits
    , "theorems" .= map (renderTheorem urlR tProps) theorems
    ]
