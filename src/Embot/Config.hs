module Embot.Config
  ( defaultLoggingSetup, stdoutAndFileLoggingSetup, emitLogMessage
  , embotDefaultConfig
  , embotDefaultLogic
  ) where

import           ClassyPrelude
import           Control.Monad.Logger (Loc, LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError, LevelOther), LogSource)
import           Control.Wire (never)
import qualified Data.Text as T
import           Language.Haskell.TH (loc_filename, loc_start)
import           System.Directory (createDirectoryIfMissing)
import           System.Log.FastLogger (LogStr, LoggerSet, newFileLoggerSet, newStdoutLoggerSet, pushLogStr, toLogStr)

import Embot.Slack (RtmStartRp)
import Embot.Types ( EmbotLogic
                   , EmbotConfig(EmbotConfig, _configurationError, _apiToken, _setupLogging, _withEnvironment)
                   , LogFunction, LoggingIO
                   , MinimalEmbotMonad )

data LogFilterLevel
  = LFLTrace
  | LFLDebug
  | LFLInfo
  | LFLWarn
  | LFLError
  deriving (Eq, Bounded, Ord)

toFilterLevel :: LogLevel -> LogFilterLevel
toFilterLevel = \ case
  LevelDebug         -> LFLDebug
  LevelInfo          -> LFLInfo
  LevelWarn          -> LFLWarn
  LevelError         -> LFLError
  LevelOther "Trace" -> LFLTrace
  _                  -> LFLTrace

defaultLoggingSetup :: IO LogFunction
defaultLoggingSetup = stdoutAndFileLoggingSetup "logs/embot.log"

stdoutAndFileLoggingSetup :: Text -> IO LogFunction
stdoutAndFileLoggingSetup path = do
  createDirectoryIfMissing False "logs"
  fileLoggerSet   <- newFileLoggerSet 16384 (unpack path)
  stdoutLoggerSet <- newStdoutLoggerSet 16384
  pure $ emitLogMessage LFLDebug {- FIXME configurable -} [fileLoggerSet, stdoutLoggerSet]

emitLogMessage :: LogFilterLevel -> [LoggerSet] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
emitLogMessage minLevel loggers loc _ level msg
  | toFilterLevel level < minLevel = pure ()
  | otherwise = do
    -- FIXME pick which messages to emit and which to suppress
    now <- getCurrentTime
    let timeText = toLogStr . T.dropEnd 6 . pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" $ now
        levelText = case level of
          LevelDebug -> "Debug"
          LevelInfo  -> "Info "
          LevelWarn  -> "Warn "
          LevelError -> "Error"
          LevelOther o -> toLogStr o
        locText = toLogStr $ loc_filename loc ++ ':' : line loc ++ ':' : char loc -- loc_package loc ++ ':' : loc_module loc ++ ' ' :
        line = show . fst . loc_start
        char = show . snd . loc_start

        str = "[" ++ timeText
          ++ " | " ++ levelText
          ++ " @ " ++ locText
          ++ "] " ++ msg
          ++ "\n"
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
