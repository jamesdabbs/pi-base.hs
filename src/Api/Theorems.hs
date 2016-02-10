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

type API = Paginated Theorem
      :<|> Body Theorem :> Authenticated :> POST (Entity Theorem)
      :<|> Capture "theorem_id" TheoremId
           :> ( GET (Entity Theorem)
           :<|> "revisions" :> Paginated Revision
           :<|> Body Theorem :> Authenticated :> PUT (Entity Theorem)
           :<|> Authenticated :> DELETE (Entity Theorem)
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

index :: Pager Theorem
index = H.getPage []

create :: Theorem -> AuthenticatedAction (Entity Theorem)
create = error "create theorem"

update :: TheoremId -> Theorem -> AuthenticatedAction (Entity Theorem)
update = error "update theorem"

delete :: TheoremId -> AuthenticatedAction (Entity Theorem)
delete = error "delete theorem"

revisions :: TheoremId -> Pager Revision
revisions = H.revisions

instance FromText TheoremId where
  fromText = H.idFromText

instance FromJSON Theorem where
  parseJSON = error "Theorem parseJSON"

instance ToJSON (Page Theorem) where
  toJSON = pageJSON "theorems" $
    \(Entity _id Theorem{..}) -> object
      [ "id" .= _id
      -- TODO
      ]

instance ToJSON (Entity Theorem) where
  toJSON (Entity _id Theorem{..}) = object
    [ "id"                .= _id
    , "description"       .= theoremDescription
    -- TODO
    ]

