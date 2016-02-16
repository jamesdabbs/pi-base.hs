{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Spaces
  ( API
  , handlers
  ) where

import Prelude hiding (show)

import Api.Base
import Api.Helpers
import Data.Aeson

import qualified Models.Space as Space

type API = Paginated Space
      :<|> Body Space :> Authenticated :> POST (Entity Space)
      :<|> Capture "space_id" SpaceId
           :> ( GET (Entity Space)
           :<|> "revisions" :> Paginated Revision
           :<|> Body Space :> Authenticated :> PUT (Entity Space)
           :<|> Authenticated :> DELETE (Entity Space)
           )

handlers :: Config -> Server API
handlers = serve $
  getPage [] :<|>
  withUser . Space.create :<|>
  ( \_id ->
    get404    _id :<|>
    revisions _id :<|>
    withUser . Space.update _id :<|>
    (withUser $ Space.delete _id)
  )

instance FromText SpaceId where
  fromText = idFromText

instance FromJSON Space where
  parseJSON = withObject "space" $ \o -> do
    spaceName            <- o .:  "name"
    spaceDescription     <- o .:  "description"
    spaceProofOfTopology <- o .:? "proof_of_topology"
    return Space{..}

instance ToJSON (Page Space) where
  toJSON = pageJSON "spaces" $
    \(Entity _id Space{..}) -> object
      [ "id"   .= _id
      , "name" .= spaceName
      ]

instance ToJSON (Entity Space) where
  toJSON (Entity _id Space{..}) = object
    [ "id"                .= _id
    , "name"              .= spaceName
    , "description"       .= spaceDescription
    , "proof_of_topology" .= spaceProofOfTopology
    ]
