module Util
  ( encodeText
  , decodeText
  ) where

import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . LBS.fromStrict . encodeUtf8

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . LBS.toStrict . encode
