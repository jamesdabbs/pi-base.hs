module Handler.User where

import Import

import qualified Handler.Base as H
import Handler.Helpers (requireAdmin)


getUsersR :: Handler Value
getUsersR = requireAdmin >> H.index [Desc UserLastLoggedInAt] id

getUserR :: UserId -> Handler Value
getUserR _id = do
  _ <- requireAdmin
  user <- runDB . get404 $ _id
  returnJson $ Entity _id user
