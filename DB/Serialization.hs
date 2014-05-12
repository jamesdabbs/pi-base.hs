{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module DB.Serialization
( dump
, load
) where

import Prelude

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist (PersistValue (PersistText), PersistField (..))


dump :: ToJSON a => a -> PersistValue
dump = PersistText . decodeUtf8 . LBS.toStrict . encode

load :: FromJSON a => PersistValue -> Either Text a
load v = case fromPersistValue v of
  Left t  -> Left t
  Right t -> case decode . LBS.fromStrict . encodeUtf8 $ t of
    Nothing -> Left "Could not decode persist value"
    Just l  -> Right l
