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
import qualified Database.Persist as DB
import Servant

import qualified Handlers.Helpers as H
import Models (runDB)
import Revisions (saveRevision)
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
handlers = H.serve $
  index  :<|>
  create :<|>
  ( \_id ->
    H.show    _id :<|>
    revisions _id :<|>
    update    _id :<|>
    delete    _id
  )

index :: Pager Property
index = H.getPage []

create :: Property -> AuthenticatedAction (Entity Property)
create p = H.withUser $ \user -> do
  _id <- runDB $ DB.insert p
  let prop = Entity _id p
  saveRevision user prop
  return prop

update :: PropertyId -> Property -> AuthenticatedAction (Entity Property)
update = error "update property"

delete :: PropertyId -> AuthenticatedAction (Entity Property)
delete = error "delete property"

revisions :: PropertyId -> Pager Revision
revisions = H.revisions

instance FromText PropertyId where
  fromText = H.idFromText

instance FromJSON Property where
  parseJSON = withObject "property" $ \o -> do
    propertyName        <- o .: "name"
    propertyDescription <- o .: "description"
    let propertyValueSetId = toSqlKey 1
        propertyAliases    = ""
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

