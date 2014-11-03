module Handler.User where

import Import


getUsersR :: Handler Html
getUsersR = error "NotImplemented - getUsersR"
  -- (users, pager) <- paged 10 [] [Desc UserLastLoggedInAt]
  -- total <- runDB $ count ([] :: [Filter User])
  -- mRev <- runDB $ selectFirst [] [Desc RevisionCreatedAt]
  -- render "Users" $(widgetFile "users/index")

getUserR :: UserId -> Handler Html
getUserR = error "NotImplemented - getUserR"
  -- user <- runDB $ get404 _id
  -- (revs, pager) <- paged 10 [RevisionUserId ==. _id] [Desc RevisionCreatedAt]
  -- render (userIdent user) $(widgetFile "users/show")
