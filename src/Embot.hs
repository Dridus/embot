module Embot where

import           ClassyPrelude (Int, IO, String, forM_, map)
import qualified Config.Dyre as Dyre
import           Control.Applicative (Applicative, (<$>), pure)
import           Control.Arrow (arr)
import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Concurrent.Chan (newChan, readChan, writeChan)
import           Control.Lens ((+=), (.=), (^.), set, use, view)
import           Control.Lens.TH (makeLenses)
import           Control.Monad (Monad, (>>=), fail, forever)
import           Control.Monad.Logger (MonadLogger, Loc, LogLevel, LogSource, LoggingT(runLoggingT), defaultLogStr, logDebug, logError, logInfo, logOther, logWarn)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Monad.Trans (MonadIO, lift, liftIO)
import           Control.Wire (Session, Timed, Wire, clockSession_, stepSession, stepWire)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent))
import qualified Data.Aeson as Aeson
import           Data.Bool (Bool(False))
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (Either(Left, Right))
import           Data.Function (($), (.), flip)
import           Data.Functor (Functor)
import           Data.List (head)
import           Data.Maybe (Maybe(Just, Nothing), maybe)
import           Data.Monoid ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as DTE
import           Data.Time.Clock (NominalDiffTime, getCurrentTime)
import           Data.Time.Format (formatTime)
import           Data.Word (Word64)
import qualified Network.Socket            as S
import           Network.URI (parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Network.Wreq (FormParam((:=)), post, asJSON, responseBody)
import qualified OpenSSL                   as SSL
import qualified OpenSSL.Session           as SSL
import           System.Directory (createDirectoryIfMissing)
import qualified System.IO.Streams         as Streams
import qualified System.IO.Streams.SSL     as Streams
import           System.Locale (defaultTimeLocale)
import           System.Log.FastLogger (LoggerSet, LogStr, newFileLoggerSet, newStdoutLoggerSet, pushLogStr, toLogStr)
import           Text.Read (read)
import qualified Text.Show as Show
import           Text.Show.Text (show)

import qualified Embot.SlackAPI as Slack
import           Embot.Action (Action(SendMessage))

import           Paths_embot (version)

-- |The `LoggingIO` monad fuses Logging with IO and is newtyped with appropriate standalone deriving clauses so that additional transformers could be fused in without `lift` towers having to be made.
newtype LoggingIO a = LoggingIO { unLoggingIO :: LoggingT IO a }
deriving instance Applicative LoggingIO
deriving instance Functor     LoggingIO
deriving instance Monad       LoggingIO
deriving instance MonadIO     LoggingIO
deriving instance MonadLogger LoggingIO

type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type EmbotLogic m = Wire (Timed NominalDiffTime ()) () m (Event Slack.RtmEvent) (Event (Seq Action))

data EmbotConfig m = EmbotConfig
    { _configurationError :: Maybe Text
    , _apiToken           :: Text
    , _setupLogging       :: IO LogFunction
    , _withEnvironment    :: forall a. (EmbotLogic m -> m a) -> LoggingIO a
    }

data EmbotState m = EmbotState
    { _stateWire           :: EmbotLogic m
    , _stateSession        :: Session m (Timed NominalDiffTime ())
    , _stateSequenceNumber :: Word64
    }

makeLenses ''EmbotConfig
makeLenses ''EmbotState

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

embotDefaultConfig :: Text -> EmbotConfig LoggingIO
embotDefaultConfig token = EmbotConfig
    { _configurationError = Nothing
    , _apiToken           = token
    , _setupLogging       = defaultLoggingSetup
    , _withEnvironment    = \ runner -> runner embotDefaultLogic
    }

embotDefaultLogic :: EmbotLogic LoggingIO
embotDefaultLogic = arr $ map respond
    where
        respond (Slack.RtmMessage message@(view Slack.messageChat -> Just chat)) =
            Seq.singleton (SendMessage chat $ show message)
        respond _ =
            Seq.empty

embot :: (Applicative m, Monad m, MonadIO m, MonadLogger m) => EmbotConfig m -> IO ()
embot = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "Embot"
    , Dyre.realMain    = embotMain
    , Dyre.showError   = flip $ set configurationError . Just . pack
    }

embotMain :: (Applicative m, Monad m, MonadIO m, MonadLogger m) => EmbotConfig m -> IO ()
embotMain config = do
    loggerFunc <- view setupLogging config
    let inLoggingIO :: LoggingIO a -> IO a
        inLoggingIO = flip runLoggingT loggerFunc . unLoggingIO
    inLoggingIO . $logInfo $ "Embot " <> (pack . Show.show $ version) <> " starting"
    forM_ (view configurationError config) $ \ error ->
        inLoggingIO . $logError $ "Error occurred during configuration reload: " <> error
    inLoggingIO . $logInfo $ "Connecting to Slack"
    post "https://slack.com/api/rtm.start" ["token" := view apiToken config]
        >>= asJSON >>= pure . view responseBody
        >>= \ case
            Slack.ResponseNotOk err -> fail . unpack $ "rtm.start failed with " <> err
            Slack.ResponseOk rp -> do
                inLoggingIO . $logDebug $ "rtm.start finished"

                let url = rp ^. Slack.rtmStartUrl
                uri <- case parseURI (unpack url) of
                    Just u -> pure u
                    Nothing  -> fail . unpack $ "failed to parse websocket URI " <> url
                uriAuth <- case uriAuthority uri of
                    Just ua -> pure ua
                    Nothing -> fail . unpack $ "no URI authority in " <> url

                wss
                    (uriRegName uriAuth)
                    (case uriPort uriAuth of { "" -> 443; other -> read other })
                    (uriPath uri)
                    (embotApp inLoggingIO config)

wss :: String -> Int -> String -> WS.ClientApp a -> IO a
wss host port path app = SSL.withOpenSSL $ do
    ctx <- SSL.context
    addrInfo  <- S.getAddrInfo Nothing (Just host) (Just $ Show.show port)
    let a = S.addrAddress $ head addrInfo
        f = S.addrFamily $ head addrInfo
    s <- S.socket f S.Stream S.defaultProtocol
    S.connect s a
    ssl <- SSL.connection ctx s
    SSL.connect ssl
    (i,o) <- Streams.sslToStreams ssl
    stream <- WS.makeStream (Streams.read i) (\ lbs -> Streams.write (LBS.toStrict <$> lbs) o)
    WS.runClientWithStream stream host path WS.defaultConnectionOptions [] app

embotApp :: (Applicative m, Monad m, MonadIO m, MonadLogger m) => (forall a. LoggingIO a -> IO a) -> EmbotConfig m -> WS.ClientApp ()
embotApp inLoggingIO config conn = inLoggingIO $ do
    $logInfo $ "RTM connected"
    $logDebug $ "entering configured environment"
    view withEnvironment config $ \ logic -> do
        $logDebug "in configured environment"
        chan <- liftIO newChan
        readerThreadId <- liftIO $ forkFinally
            (inLoggingIO . forever $ do
                $(logOther "Trace") "waiting for RTM message"
                msg <- liftIO $ WS.receiveData conn
                $(logOther "Trace") $ DTE.decodeUtf8 msg
                case Aeson.eitherDecode' (LBS.fromStrict msg) of
                    Left failure ->
                        $logWarn $ "failed to parse: " <> pack failure
                    Right slackEvent -> do
                        $(logOther "Trace") $ " => " <> show (slackEvent :: Slack.RtmEvent)
                        liftIO . writeChan chan . Just $ slackEvent
            )
            (\ result -> inLoggingIO $ case result of
                Left exception -> do
                    $logError $ "reader thread failed: " <> (pack . Show.show $ exception)
                Right () -> do
                    $logWarn $ "reader thread ended"
            )

        idlerThreadId <- liftIO . forkIO . forever $ do
            writeChan chan Nothing
            threadDelay 1000000

        let _ = readerThreadId
            _ = idlerThreadId

        let initialSession = clockSession_
            initialState = EmbotState
                { _stateWire = logic
                , _stateSession = initialSession
                , _stateSequenceNumber = 1
                }

        flip evalStateT initialState . forever $ do
            slackEventMaybe <- liftIO $ readChan chan
            wire <- use stateWire
            sess <- use stateSession
            (s, sess') <- lift $ stepSession sess
            stateSession .= sess'
            (result, wire') <- lift $ stepWire wire s (maybe (Right NoEvent) (Right . Event) slackEventMaybe)
            stateWire .= wire'
            case result of
                Right (Event actions) -> forM_ actions $ \ case
                    SendMessage chat message -> do
                        seqnum <- use stateSequenceNumber
                        liftIO $ WS.sendTextData conn (Aeson.encode $ Slack.RtmSendMessage seqnum chat message)
                        stateSequenceNumber += 1
                _ -> pure ()
