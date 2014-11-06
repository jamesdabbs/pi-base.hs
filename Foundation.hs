module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail2
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

import Control.Monad (unless)
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Rollbar
import Rollbar.MonadLogger (reportErrorS)


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    , appRollbar :: Rollbar.Settings
    , appGoogleId :: T.Text
    , appGoogleSecret :: T.Text
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

isLoggedIn :: Handler AuthResult
isLoggedIn = do
  ma <- maybeAuth
  return $ case ma of
    Nothing -> AuthenticationRequired
    _       -> Authorized

isAdmin :: Handler AuthResult
isAdmin = do
  ma <- maybeAuth
  return $ case fmap (userAdmin . entityVal) ma of
    Nothing    -> AuthenticationRequired
    Just True  -> Authorized
    Just False -> Unauthorized "You must be an admin"

getCurrentPath :: Handler (Maybe T.Text)
getCurrentPath = do
  renderer <- getUrlRender
  route <- getCurrentRoute
  return $ fmap renderer route

handleError :: (Show e) => e -> Handler TypedContent
handleError err = selectRep $ do
  provideRep . defaultLayout $ do
    setTitle "Server Error | π-Base"
    $(widgetFile "500")
  provideRep . return . object $
    [ "status" .= (500 :: Int)
    , "error"  .= show err
    ]

handleMissing :: Handler TypedContent
handleMissing = selectRep $ do
  provideRep . defaultLayout $ do
    setTitle "404 | π-Base"
    $(widgetFile "404")
  provideRep . return . object $
    [ "status" .= (404 :: Int)
    , "error"  .= ("Could not find the requested resource" :: T.Text)
    ]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        ma   <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_main_css
                ])

            unless development $ addScript $ StaticR js_rollbar_js

            $(combineScripts 'StaticR
              [ vendor_js_jquery_js
              , vendor_js_bootstrap_js
              , vendor_js_typeahead_js
              , vendor_js_markdown_js
              , vendor_js_underscore_js
              , js_jsonlite_js
              , js_latinize_js
              , js_local_cache_js
              , js_markdown_js
              , js_pi_base_js
              , js_property_js
              , js_formula_typeahead_js
              , js_formula_js
              ])

            $(widgetFile "default-layout")

        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Must be an admin to delete
    isAuthorized (DeleteSpaceR    _) _ = isAdmin
    isAuthorized (DeletePropertyR _) _ = isAdmin
    isAuthorized (DeleteTraitR    _) _ = isAdmin
    isAuthorized (DeleteTheoremR  _) _ = isAdmin

    -- Must be an admin to inspect revisions
    isAuthorized (SpaceRevisionsR    _) _ = isAdmin
    isAuthorized (PropertyRevisionsR _) _ = isAdmin
    isAuthorized (TraitRevisionsR    _) _ = isAdmin
    isAuthorized (TheoremRevisionsR  _) _ = isAdmin

    -- Must be an admin for admin functions
    isAuthorized AdminR           _ = isAdmin
    isAuthorized ContradictionsR  _ = isAdmin
    isAuthorized ExploreR         _ = isAdmin
    isAuthorized ResetR           _ = isAdmin
    isAuthorized TraitProgressR   _ = isAdmin
    isAuthorized TheoremProgressR _ = isAdmin

    -- Must be an admin to administrate users
    isAuthorized  UsersR   _ = isAdmin
    isAuthorized (UserR _) _ = isAdmin

    -- Must be logged in to create
    isAuthorized CreateSpaceR     _ = isLoggedIn
    isAuthorized CreatePropertyR  _ = isLoggedIn
    isAuthorized CreateTheoremR   _ = isLoggedIn
    isAuthorized (CreateTraitR _) _ = isLoggedIn

    -- Must be logged in to edit
    isAuthorized (EditSpaceR    _) _ = isLoggedIn
    isAuthorized (EditPropertyR _) _ = isLoggedIn
    isAuthorized (EditTraitR    _) _ = isLoggedIn
    isAuthorized (EditTheoremR  _) _ = isLoggedIn
    isAuthorized (SpaceR        _) _ = isLoggedIn
    isAuthorized (PropertyR     _) _ = isLoggedIn
    isAuthorized (TraitR        _) _ = isLoggedIn
    isAuthorized (TheoremR      _) _ = isLoggedIn

    -- Must be an admin for any other write request
    isAuthorized _ True = isAdmin
    isAuthorized _ _    = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    errorHandler err@(InternalError e) = do
      app <- getYesod
      unless development $ forkHandler ($logErrorS "errorHandler" . T.pack . show) $ do
          muser <- maybeAuth
          path <- getCurrentPath
          let rollbarPerson (Entity uid user) =
                 Rollbar.Person
                   { Rollbar.id       = toPathPiece uid
                   , Rollbar.username = Nothing
                   , Rollbar.email    = Just $ userIdent user
                   }
          let rPerson = fmap rollbarPerson muser
          reportErrorS (appRollbar app)
                       (Rollbar.Options rPerson Nothing)
                       (fromMaybe "errorHandler" path)
                       ($logDebugS) e
      handleError err
    errorHandler NotFound = handleMissing
    errorHandler err = defaultErrorHandler err

authGoogleEmail' :: App -> AuthPlugin App
authGoogleEmail' app = authGoogleEmail (appGoogleId app) (appGoogleSecret app)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        now <- liftIO $ getCurrentTime
        case x of
            Just (Entity uid _) -> do
              update uid [UserLastLoggedInAt =. now]
              return $ Just uid
            Nothing -> do
                fmap Just $ insert User
                    { userIdent = credsIdent creds
                    , userName = Nothing
                    , userAdmin = False
                    , userCreatedAt = now
                    , userLastLoggedInAt = now
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins app = [authBrowserId def, authGoogleEmail' app]

    authHttpManager = httpManager

    onLogin = setMessage [shamlet|<.alert.alert-success>You are now logged in|]

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
