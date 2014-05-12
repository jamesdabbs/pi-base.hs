module Util
( unionN
, intersectionN
, encodeText
, decodeText
) where

import Prelude

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)


intersectionN :: Ord a => [S.Set a] -> S.Set a
intersectionN [] = S.empty
intersectionN ls = foldl1 S.intersection ls

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl S.union S.empty

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . LBS.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . LBS.toStrict . encode
