module Model.Revision
( revisionCreate
) where

import Import
import Yesod.Auth (requireAuthId)

import Data.Time (getCurrentTime)

revisionCreate :: Text -> Maybe RevisionId -> Handler RevisionId
revisionCreate text parent = do
  auth <- requireAuthId
  now  <- liftIO getCurrentTime
  runDB . insert $ Revision
    { revisionText = text
    , revisionParent = parent
    , revisionUserId = auth
    , revisionCreatedAt = now
    }
