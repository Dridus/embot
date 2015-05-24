module Embot.Config
    ( defaultLoggingSetup, stdoutAndFileLoggingSetup, emitLogMessage
    , embotDefaultConfig
    , embotDefaultLogic
    ) where

import ClassyPrelude (forM_)
import Control.Applicative (pure)
import Control.Category ((.))
import Control.Monad.Logger (Loc, LogLevel, LogSource, defaultLogStr)
import Control.Wire (never)
import Data.Bool (Bool(False))
import Data.Function (($))
import Data.Maybe (Maybe(Nothing))
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Directory (createDirectoryIfMissing)
import System.IO (IO)
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger (LogStr, LoggerSet, newFileLoggerSet, newStdoutLoggerSet, pushLogStr, toLogStr)

import Embot.Types
    ( EmbotLogic
    , EmbotConfig(EmbotConfig, _configurationError, _apiToken, _setupLogging, _withEnvironment)
    , LogFunction, LoggingIO
    , MinimalEmbotMonad
    )

defaultLoggingSetup :: IO LogFunction
defaultLoggingSetup = stdoutAndFileLoggingSetup "logs/embot.log"

stdoutAndFileLoggingSetup :: Text -> IO LogFunction
stdoutAndFileLoggingSetup path = do
    createDirectoryIfMissing False "logs"
    fileLoggerSet   <- newFileLoggerSet 16384 (unpack path)
    stdoutLoggerSet <- newStdoutLoggerSet 16384
    pure $ emitLogMessage [fileLoggerSet, stdoutLoggerSet]

emitLogMessage :: [LoggerSet] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
emitLogMessage loggers loc src level msg = do
    -- FIXME pick which messages to emit and which to suppress
    now <- getCurrentTime
    let str = "[" <> (toLogStr . pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" $ now) <> "] " <> defaultLogStr loc src level msg
    forM_ loggers $ \ logger ->
        pushLogStr logger str

embotDefaultConfig :: MinimalEmbotMonad m => Text -> (forall a. (EmbotLogic m -> m a) -> LoggingIO a) -> EmbotConfig m
embotDefaultConfig token withEnv = EmbotConfig
    { _configurationError = Nothing
    , _apiToken           = token
    , _setupLogging       = defaultLoggingSetup
    , _withEnvironment    = withEnv
    }

embotDefaultLogic :: EmbotLogic LoggingIO
embotDefaultLogic = never
