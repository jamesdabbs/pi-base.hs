{-# LANGUAGE OverloadedStrings #-}

module Util
  ( encodeText
  , decodeText
  , err422
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Servant (ServantErr(..))

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
