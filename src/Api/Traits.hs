{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Traits
  ( API
  , handlers
  ) where

import Api.Base
import Api.Helpers
import Data.Aeson

import qualified Models.Trait as Trait

type API = Paginated Trait
      :<|> Body Trait :> Authenticated :> POST (Entity Trait)
      :<|> Capture "trait_id" TraitId
           :> ( GET (Entity Trait)
           :<|> "revisions" :> Paginated Revision
           :<|> Body Trait :> Authenticated :> PUT (Entity Trait)
           :<|> Authenticated :> DELETE (Entity Trait)
           )

handlers :: Config -> Server API
handlers = serve $
  getPage [] :<|>
  withUser . Trait.create :<|>
  ( \_id ->
    get404    _id :<|>
    revisions _id :<|>
    withUser . Trait.update _id :<|>
    (withUser $ Trait.delete _id)
  )

instance FromText TraitId where
  fromText = idFromText

instance FromJSON Trait where
  parseJSON = error "Trait parseJSON"

instance ToJSON (Page Trait) where
  toJSON = pageJSON "traits" $
    \(Entity _id Trait{..}) -> object
      [ "id" .= _id
      -- TODO
      ]

instance ToJSON (Entity Trait) where
  toJSON (Entity _id Trait{..}) = object
    [ "id"                .= _id
    , "description"       .= traitDescription
    -- TODO
    ]
