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


type API = GET [Entity Trait]
      :<|> Body Trait :> Authenticated :> POST (Entity Trait)
      :<|> Capture "trait_id" TraitId
           :> ( GET (Entity Trait)
           :<|> Body Trait :> Authenticated :> PUT (Entity Trait)
           :<|> Authenticated :> DELETE (Entity Trait)
           :<|> "revisions" :> GET [Rev Trait]
           )

handlers :: Config -> Server API
handlers = H.serve $
  index  :<|>
  create :<|>
  ( \_id ->
    H.show    _id :<|>
    update    _id :<|>
    delete    _id :<|>
    revisions _id
  )


index :: Action [Entity Trait]
index = H.index

create :: Trait -> AuthenticatedAction (Entity Trait)
create = error "create trait"

update :: TraitId -> Trait -> AuthenticatedAction (Entity Trait)
update = error "update trait"

delete :: TraitId -> AuthenticatedAction (Entity Trait)
delete = error "delete trait"

revisions :: TraitId -> Action [Rev a]
revisions = error "trait revisions"

instance FromText TraitId where
  fromText = H.idFromText

instance FromJSON Trait where
  parseJSON = error "Trait parseJSON"

instance ToJSON [Entity Trait] where
  toJSON ps = object [ "traits" .= map fmt ps ]
    where
      fmt (Entity _id Trait{..}) = object
        [ "id" .= _id
        -- TODO
        ]

instance ToJSON (Entity Trait) where
  toJSON (Entity _id Trait{..}) = object
    [ "id"                .= _id
    , "description"       .= traitDescription
    -- TODO
    ]
