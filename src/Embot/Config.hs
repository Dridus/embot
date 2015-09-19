module Embot.Config
  ( defaultLoggingSetup, stdoutAndFileLoggingSetup, emitLogMessage
  , embotDefaultConfig
  , embotDefaultLogic
  ) where

import           ClassyPrelude
import           Control.Monad.Logger (Loc, LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError, LevelOther), LogSource)
import           Control.Wire (never)
import qualified Data.Text as T
import           Language.Haskell.TH (loc_package, loc_module, loc_filename, loc_start)
import           System.Directory (createDirectoryIfMissing)
import           System.Log.FastLogger (LogStr, LoggerSet, newFileLoggerSet, newStdoutLoggerSet, pushLogStr, toLogStr)

import Embot.Slack (RtmStartRp)
import Embot.Types ( EmbotLogic
                   , EmbotConfig(EmbotConfig, _configurationError, _apiToken, _setupLogging, _withEnvironment)
                   , LogFunction, LoggingIO
                   , MinimalEmbotMonad )

defaultLoggingSetup :: IO LogFunction
defaultLoggingSetup = stdoutAndFileLoggingSetup "logs/embot.log"

stdoutAndFileLoggingSetup :: Text -> IO LogFunction
stdoutAndFileLoggingSetup path = do
  createDirectoryIfMissing False "logs"
  fileLoggerSet   <- newFileLoggerSet 16384 (unpack path)
  stdoutLoggerSet <- newStdoutLoggerSet 16384
  pure $ emitLogMessage [fileLoggerSet, stdoutLoggerSet]

emitLogMessage :: [LoggerSet] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
emitLogMessage loggers loc _ level msg = do
  -- FIXME pick which messages to emit and which to suppress
  now <- getCurrentTime
  let timeText = toLogStr . T.dropEnd 6 . pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" $ now
      levelText = case level of
        LevelDebug -> "Debug"
        LevelInfo  -> "Info "
        LevelWarn  -> "Warn "
        LevelError -> "Error"
        LevelOther o -> toLogStr o
      locText = toLogStr $ loc_package loc ++ ':' : loc_module loc ++
                          ' ' : loc_filename loc ++ ':' : line loc ++ ':' : char loc
      line = show . fst . loc_start
      char = show . snd . loc_start

      str = "[" ++ timeText
         ++ " | " ++ levelText
         ++ "] " ++ msg
         ++ "   @(" ++ locText ++ ")\n"
  forM_ loggers $ \ logger ->
    pushLogStr logger str

embotDefaultConfig :: MinimalEmbotMonad m => Text -> (forall a. RtmStartRp -> (EmbotLogic m -> m a) -> LoggingIO a) -> EmbotConfig m
embotDefaultConfig token withEnv = EmbotConfig
  { _configurationError = Nothing
  , _apiToken           = token
  , _setupLogging       = defaultLoggingSetup
  , _withEnvironment    = withEnv }

embotDefaultLogic :: EmbotLogic LoggingIO
embotDefaultLogic = never
