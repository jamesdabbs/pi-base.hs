module Handler.Test
( getTestR
, postTestResetR
) where

import Import

import qualified Data.Text as T
import Database.Persist.Sql (rawExecute)

import Handler.Helpers (developmentOnly)


getTestR :: Handler Value
getTestR = developmentOnly $ do
  returnJson $ object [ "status" .= ("ok" :: Text) ]

postTestResetR :: Handler Value
postTestResetR = developmentOnly $ do
  now <- liftIO getCurrentTime

  runDB $ rawExecute "TRUNCATE struts, assumptions, supporters, proofs, revisions, theorem_properties, traits, properties, spaces, theorems, emails, remote_users RESTART IDENTITY CASCADE" []

  (Entity boolId _) <- runDB . getBy404 $ UValueSetName "boolean"

  _ <- runDB $ do
    insert_ $ User "admin" (Just "admin") True  now now
    insert_ $ User "user"  (Just "user" ) False now now

    forM_ ([1..100] :: [Integer])$ \n -> do
      let n' = T.pack $ show n
      insert_ $ Space ("Space " <> n') (Textarea "-") now now Nothing
      insert_ $ Property ("Property " <> n') [] (Textarea "-") boolId now now

  returnJson $ object [ "status" .= ("Ok" :: Text) ]
