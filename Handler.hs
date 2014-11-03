module Handler where

import Import

getHomeR :: Handler Value
getHomeR = returnJson $ object [ "status" .= ("ok" :: Text) ]
