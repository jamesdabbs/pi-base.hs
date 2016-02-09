{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Theorems
  ( API
  , handlers
  ) where

import Base
import Data.Aeson
import Servant

import qualified Handlers.Helpers as H

type API = GET [Entity Theorem]
      :<|> Body Theorem :> Authenticated :> POST (Entity Theorem)
      :<|> Capture "theorem_id" TheoremId
           :> ( GET (Entity Theorem)
           :<|> Body Theorem :> Authenticated :> PUT (Entity Theorem)
           :<|> Authenticated :> DELETE (Entity Theorem)
           :<|> "revisions" :> GET [Rev Theorem]
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

index :: Action [Entity Theorem]
index = H.index

create :: Theorem -> AuthenticatedAction (Entity Theorem)
create = error "create theorem"

update :: TheoremId -> Theorem -> AuthenticatedAction (Entity Theorem)
update = error "update theorem"

delete :: TheoremId -> AuthenticatedAction (Entity Theorem)
delete = error "delete theorem"

revisions :: TheoremId -> Action [Rev a]
revisions = error "theorem revisions"

instance FromText TheoremId where
  fromText = H.idFromText

instance FromJSON Theorem where
  parseJSON = error "Theorem parseJSON"

instance ToJSON [Entity Theorem] where
  toJSON ps = object [ "spaces" .= map fmt ps ]
    where
      fmt (Entity _id Theorem{..}) = object
        [ "id" .= _id
        -- TODO
        ]

instance ToJSON (Entity Theorem) where
  toJSON (Entity _id Theorem{..}) = object
    [ "id"                .= _id
    , "description"       .= theoremDescription
    -- TODO
    ]

