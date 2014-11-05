module Handler.Helpers
( preview
, plural
, render
, developmentOnly
, requireUser
, requireAdmin
, requireGetParam
, flash
, FlashClass (..)
, sendErrorMessage
, invalid422
) where

import Import

import Data.Char (toLower)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types
import System.Posix.Env (getEnv)

render :: Text -> Widget -> Handler Html
render title w = defaultLayout $ do
  setTitle . toHtml $ [shamlet|#{title} | π-Base|]
  w


plural :: Int -> Text -> Text
plural 1 x = "1 " <> x
plural n x = (T.pack $ show n) <> " " <> x


-- WARNING: this _sets_ X-Total-Count, and so may not work as expected if called multiple times in one handler

preview :: Textarea -> Text
preview t = case T.lines . unTextarea $ t of
  (l:_) -> l
  _     -> ""


data FlashClass = Success | Info | Warning | Danger deriving (Show)

flash :: FlashClass -> Text -> Handler ()
flash c msg = setMessage [shamlet|<.alert.alert-#{map toLower $ show c}>#{msg}|]

sendErrorMessage :: MonadHandler m => Status -> Text -> m a
sendErrorMessage status msg = sendResponseStatus status $ object [ "error" .= msg ]

sendError :: MonadHandler m => Status -> m a
sendError status = sendErrorMessage status (decodeUtf8 . statusMessage $ status)

requireUser :: Handler (Entity User)
requireUser = do
  let unauthed = sendError unauthorized401
  authHeader <- lookupHeader "Authorization"
  case authHeader of
    Nothing -> unauthed
    -- FIXME! - secure this auth!
    Just token -> do
      muser <- runDB . getBy . UniqueUser . decodeUtf8 $ token
      case muser of
        Nothing -> unauthed
        Just user -> return user

requireAdmin :: Handler (Entity User)
requireAdmin = do
  user <- requireUser
  case userAdmin . entityVal $ user of
    True  -> return user
    False -> sendError forbidden403

developmentOnly :: Handler a -> Handler a
developmentOnly h = do
  mode <- liftIO $ getEnv "YESOD_ENV"
  case mode of
    Just "development" -> h
    _ -> sendErrorMessage forbidden403 "This action is only available in development"

invalid422 :: Status
invalid422 = Status 422 "Invalid"

requireGetParam :: MonadHandler m => Text -> m Text
requireGetParam key = do
  mval <- lookupGetParam key
  case mval of
    Nothing  -> sendError invalid422
    Just val -> return val
