{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Properties
  ( API
  , handlers
  ) where

import Base
import Data.Aeson
import Servant

import qualified Handlers.Helpers as H
import Util (forceKey)

type API = GET [Entity Property]
     :<|> Body Property :> Authenticated :> POST (Entity Property)
     :<|> Capture "property_id" PropertyId
          :> ( GET (Entity Property)
          :<|> Body Property :> Authenticated :> PUT (Entity Property)
          :<|> Authenticated :> DELETE (Entity Property)
          :<|> "revisions" :> GET [Rev Property]
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

index :: Action [Entity Property]
index = H.index

create :: Property -> AuthenticatedAction (Entity Property)
create = error "create property"

update :: PropertyId -> Property -> AuthenticatedAction (Entity Property)
update = error "update property"

delete :: PropertyId -> AuthenticatedAction (Entity Property)
delete = error "delete property"

revisions :: PropertyId -> Action [Rev a]
revisions = error "property revisions"

instance FromText PropertyId where
  fromText = H.idFromText

instance FromJSON Property where
  parseJSON = withObject "property" $ \o -> do
    propertyName        <- o .: "name"
    propertyDescription <- o .: "description"
    let propertyCreatedAt  = Nothing
        propertyUpdatedAt  = Nothing
        propertyValueSetId = forceKey 1
        propertyAliases    = ""
    return Property{..}

instance ToJSON [Entity Property] where
  toJSON ps = object [ "properties" .= map fmt ps ]
    where
      fmt (Entity _id Property{..}) = object
        [ "id"   .= _id
        , "name" .= propertyName
        ]

instance ToJSON (Entity Property) where
  toJSON (Entity _id Property{..}) = object
    [ "id"                .= _id
    , "name"              .= propertyName
    , "description"       .= propertyDescription
    ]

