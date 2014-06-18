module Handler.Explore where

import Import
import Yesod.Routes.Class (Route)

import qualified Data.Map as M

import DB (derivedTraits, Prefetch)
import Models
import Presenter.Theorem (theoremTitle)
import Presenter.Trait (traitTitle)

type StrutLookup = M.Map TraitId [TheoremId]
type R = Route (HandlerSite Handler) -> Text

fixme :: [Text]
fixme = ["FIXME"]

renderTrait :: R -> Prefetch Space -> Prefetch Property -> StrutLookup -> Entity Trait -> Value
renderTrait urlR ss ps struts (Entity _id t) = object
  [ "_id" .= _id
  , "name" .= traitTitle ss ps t
  , "url" .= (urlR $ TraitR _id)
  , "support" .= fixme
  , "assumptions" .= object
    [ "theorems" .= M.lookup _id struts
    , "traits" .= fixme
    ]
  ]

renderTheorem :: R -> Prefetch Property -> Entity Theorem -> Value
renderTheorem urlR ps (Entity _id t) = object
  [ "_id" .= _id
  , "name" .= theoremTitle ps t
  , "url" .= (urlR $ TheoremR _id)
  ]

gather :: StrutLookup -> Strut -> StrutLookup
gather h s = M.insert (strutTraitId s) nval h
  where
    nval = case M.lookup (strutTraitId s) h of
      Just list -> (strutTheoremId s) : list
      Nothing   -> [strutTheoremId s]

getTraitDataR :: TraitId -> Handler Value
getTraitDataR _id = do
  urlR     <- getUrlRender
  trait    <- runDB $ get404 _id
  derived  <- derivedTraits _id
  supports <- traitSupport _id
  let traits = [Entity _id trait] ++ derived ++ supports
  (spaces, properties) <- traitPrefetch traits
  struts <- runDB $ selectList [StrutTraitId <-. map entityKey traits] []
  let strutLookup = foldl gather M.empty $ map entityVal struts
  theorems <- runDB $ selectList [TheoremId <-. map (strutTheoremId . entityVal) struts] []
  tProps <- theoremPrefetch . map entityVal $ theorems
  returnJson $ object
    [ "root" .= map entityKey supports
    , "active" .= _id
    , "derived" .= map entityKey derived
    , "traits" .= map (renderTrait urlR spaces properties strutLookup) traits
    , "theorems" .= map (renderTheorem urlR tProps) theorems
    ]
