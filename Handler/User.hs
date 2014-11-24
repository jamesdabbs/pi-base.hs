module Handler.User where

import Import


import Model.Revision (revisionLink, revisionPrettyBody)
import Handler.Helpers

getUsersR :: Handler Html
getUsersR = do
  (users, pager) <- paged 10 [] [Desc UserLastLoggedInAt]
  total <- runDB $ count ([] :: [Filter User])
  mRev <- runDB $ selectFirst [] [Desc RevisionCreatedAt]
  render "Users" $(widgetFile "users/index")

getUserR :: UserId -> Handler Html
getUserR _id = do
  user <- runDB $ get404 _id
  (revs, pager) <- paged 10 [RevisionUserId ==. _id] [Desc RevisionCreatedAt]
  render (userIdent user) $(widgetFile "users/show")
