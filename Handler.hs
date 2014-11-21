module Handler where

import Import

getHomeR :: Handler Value
getHomeR = do
  env <- getYesodEnv
  returnJson $ object
    [ "environment".= (show env)
    , "tagline"    .= ("Mathematical!" :: Text)
    , "version"    .= (1 :: Int)
    ]
