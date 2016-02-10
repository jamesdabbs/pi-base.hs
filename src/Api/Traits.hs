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

import Base
import Data.Aeson
import Servant

import qualified Handlers.Helpers as H


type API = Paginated Trait
      :<|> Body Trait :> Authenticated :> POST (Entity Trait)
      :<|> Capture "trait_id" TraitId
           :> ( GET (Entity Trait)
           :<|> "revisions" :> Paginated Revision
           :<|> Body Trait :> Authenticated :> PUT (Entity Trait)
           :<|> Authenticated :> DELETE (Entity Trait)
           )

handlers :: Config -> Server API
handlers = H.serve $
  index  :<|>
  create :<|>
  ( \_id ->
    H.show    _id :<|>
    revisions _id :<|>
    update    _id :<|>
    delete    _id
  )


index :: Pager Trait
index = H.getPage []

create :: Trait -> AuthenticatedAction (Entity Trait)
create = error "create trait"

update :: TraitId -> Trait -> AuthenticatedAction (Entity Trait)
update = error "update trait"

delete :: TraitId -> AuthenticatedAction (Entity Trait)
delete = error "delete trait"

revisions :: TraitId -> Pager Revision
revisions = H.revisions

instance FromText TraitId where
  fromText = H.idFromText

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
