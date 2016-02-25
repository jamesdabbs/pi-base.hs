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

import Api.Base
import Api.Helpers
import Data.Aeson

import qualified Models.Theorem as Theorem

type API = Paginated Theorem
      :<|> Body Theorem :> Authenticated :> POST (Entity Theorem)
      :<|> Capture "theorem_id" TheoremId
           :> ( GET (Entity Theorem)
           :<|> "revisions" :> Paginated Revision
           :<|> Body Theorem :> Authenticated :> PUT (Entity Theorem)
           :<|> Authenticated :> DELETE (Entity Theorem)
           )

handlers :: ServerT API Handler
handlers = getPage []
      :<|> withUser . Theorem.create
      :<|> ( \_id -> get404 _id
                :<|> revisions _id
                :<|> withUser . Theorem.update _id
                :<|> (withUser $ Theorem.delete _id)
           )

instance FromText TheoremId where
  fromText = idFromText

instance FromJSON Theorem where
  parseJSON = error "Theorem parseJSON"

instance ToJSON (Page Theorem) where
  toJSON = pageJSON "theorems" $
    \(Entity _id Theorem{..}) -> object
      [ "id"          .= _id
      , "antecedent"  .= theoremAntecedent
      , "consequent"  .= theoremConsequent
      ]

instance ToJSON (Entity Theorem) where
  toJSON (Entity _id Theorem{..}) = object
    [ "id"          .= _id
    , "antecedent"  .= theoremAntecedent
    , "consequent"  .= theoremConsequent
    , "description" .= theoremDescription
    ]

