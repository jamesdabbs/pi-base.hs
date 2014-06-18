module Handler where

import Import

import Handler.Helpers (render)
import Handler.Partials (searchHelp)


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "π-Base"
  $(widgetFile "homepage")

getHelpR :: Handler Html
getHelpR = render "Help" $(widgetFile "help/site")
