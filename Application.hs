{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
( makeApplication
, getApplicationDev
, makeFoundation
) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Client.Conduit (newManager)
import Control.Monad.Logger (runLoggingT)
import Control.Concurrent (forkIO, threadDelay)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.Wai (Middleware)
import Network.Wai.Internal (Response(..))
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(ExitSuccess))

import qualified Rollbar
import qualified Data.Text as T

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler
import Handler.Admin
import Handler.Explore
import Handler.Properties
import Handler.Search
import Handler.Spaces
import Handler.Theorems
import Handler.Traits
import Handler.User

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


addHeaders :: ResponseHeaders -> Middleware
addHeaders hs app env = do
    res <- app env
    return $ case res of
        ResponseFile s rhs f mfp -> ResponseFile s (fix rhs) f mfp
        ResponseBuilder s rhs b -> ResponseBuilder s (fix rhs) b
        ResponseSource s rhs src -> ResponseSource s (fix rhs) src
        r -> r
  where fix rhs = rhs ++ hs

cors :: Middleware
cors = addHeaders [("Access-Control-Allow-Origin", "http://localhost:8000")]


-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (cors . logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, updater) <- clockDateCacher


    (code, rev, _) <- readProcessWithExitCode "git" ["rev-parse", "--short", "HEAD"] []
    let build = case code of
                  ExitSuccess -> Just rev
                  _ -> Nothing

    let x = appExtra conf
    let rc = Rollbar.Settings
            { Rollbar.environment = Rollbar.Environment . T.pack . show $ appEnv conf
            , Rollbar.token = Rollbar.ApiToken . extraRollbarToken $ x
            , Rollbar.hostName = "pi-base | v" ++ (show build)
            }

    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    let gClientId = extraGoogleClientId x
    let gSecret = extraGoogleSecretKey x

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger rc gClientId gSecret build

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
