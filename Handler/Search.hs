module Handler.Search where

import Import

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding (encodeUtf8)

import Logic.Types (Formula(..), MatchType(..))
import Logic (matches)

jsonError :: Text -> Handler Value
jsonError str = returnJson $ object [ "error" .= str ]

decodeText :: FromJSON a => Text -> Maybe a
decodeText = decode . LBS.fromStrict . encodeUtf8

getSearchR :: Handler Value
getSearchR = do
  q <- lookupGetParam "q"
  case q of
    Nothing -> jsonError "No `q` given"
    Just qt -> do
      $(logDebug) $ qt
      case decodeText qt of
        Nothing -> jsonError "Could not parse formula"
        Just f -> do
          spaces <- matches Yes f
          returnJson spaces
