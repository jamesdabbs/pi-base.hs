{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util
  ( encodeText
  , decodeText
  , err422
  , intersectN
  , unionN
  , forceKey
  , flatMapM
  ) where

import Data.Aeson
import Data.Int           (Int64)
import Data.Text          (Text, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Database.Persist   (Key, PersistEntity(..), PersistValue(..), keyFromValues)
import Servant            (ServantErr(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set             as S

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . LBS.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . LBS.toStrict . encode

instance ToJSON ServantErr where
  toJSON err = object
    [ "status" .= (errHTTPCode err)
    , "error"  .= (decodeUtf8 . LBS.toStrict $ errBody err)
    ]

err422 :: ServantErr
err422 = ServantErr
  { errHTTPCode = 422
  , errReasonPhrase = "Invalid"
  , errBody = ""
  , errHeaders = []
  }

intersectN :: Ord a => [S.Set a] -> S.Set a
intersectN [] = S.empty
intersectN ls = foldl1 S.intersection ls

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl S.union S.empty

forceKey :: PersistEntity rec => Int64 -> Key rec
forceKey n = case keyFromValues [PersistInt64 n] of
  Right key -> key
  Left    e -> error $ unpack e

flatMapM :: (Monad m, Traversable t) => (a -> m [b]) -> t a -> m [b]
flatMapM f m = mapM f m >>= return . concat
