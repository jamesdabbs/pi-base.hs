module Handler.Search where

import Import

import Logic (matches)
import Util (decodeText)

jsonError :: Text -> Handler Value
jsonError str = returnJson $ object [ "error" .= str ]

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
