module Handler where

import Import

import Data.FileEmbed (embedFile)
import Handler.Helpers (render)
import Handler.Partials (searchHelp)


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "π-Base"
  $(widgetFile "homepage")

getHelpR :: Handler Html
getHelpR = render "Help" $(widgetFile "help/site")

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
