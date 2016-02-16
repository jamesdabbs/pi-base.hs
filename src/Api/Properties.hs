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

import Api.Base
import Data.Aeson
import qualified Models.Property as Property

import Api.Helpers
import Util (toSqlKey)

type API = Paginated Property
     :<|> Body Property :> Authenticated :> POST (Entity Property)
     :<|> Capture "property_id" PropertyId
          :> ( GET (Entity Property)
          :<|> "revisions" :> Paginated Revision
          :<|> Body Property :> Authenticated :> PUT (Entity Property)
          :<|> Authenticated :> DELETE (Entity Property)
          )

handlers :: Config -> Server API
handlers = serve $
  getPage [] :<|>
  withUser . Property.create :<|>
  ( \_id ->
    get404    _id :<|>
    revisions _id :<|>
    withUser . Property.update _id :<|>
    (withUser $ Property.delete _id)
  )

instance FromText PropertyId where
  fromText = idFromText

instance FromJSON Property where
  parseJSON = withObject "property" $ \o -> do
    propertyName        <- o .: "name"
    propertyDescription <- o .: "description"
    let propertyValueSetId = toSqlKey 1
        propertyAliases    = []
    return Property{..}

instance ToJSON (Page Property) where
  toJSON = pageJSON "properties" $
    \(Entity _id Property{..}) -> object
      [ "id"   .= _id
      , "name" .= propertyName
      ]

instance ToJSON (Entity Property) where
  toJSON (Entity _id Property{..}) = object
    [ "id"                .= _id
    , "name"              .= propertyName
    , "description"       .= propertyDescription
    ]
