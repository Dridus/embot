module Main where

import           Control.Applicative ((<$>), pure)
import           Control.Lens ((+=), (.=), (^.), use, view)
import           Control.Lens.TH (makeLenses)
import           Control.Monad ((>>=), fail, forever, forM_)
import           Control.Monad.Logger (Loc, LogLevel, LogSource, LoggingT(runLoggingT), defaultLogStr, logDebug, logInfo, logOther, logWarn)
import           Control.Monad.RWS.Strict (execRWST)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans (lift, liftIO)
import qualified Data.Aeson as Aeson
import           Data.Bool (Bool(False))
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (Either(Left, Right))
import           Data.Function (($), (.), flip)
import           Data.HList (HList)
import           Data.HList.TIP (TIP)
import           Data.List (head)
import           Data.Maybe (Maybe(Just, Nothing))
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack)
import qualified Data.Text.Encoding as DTE
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime)
import           Data.Word (Word64)
import qualified Network.Socket            as S
import           Network.URI (parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Network.Wreq (FormParam((:=)), post, asJSON, responseBody)
import qualified OpenSSL                   as SSL
import qualified OpenSSL.Session           as SSL
import           Prelude (Int, IO, String)
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
import           Embot.Core (EmbotIO(unEmbotIO), Env, InterceptorState(InterceptorState), InterceptorWithState, readGlobalConfig, apiToken)
import           Embot.Event (Event(Event), EventDetail(ReceivedMessage))
import           Embot.Plugins (initializeEnv, initializeInterceptors)

import           Paths_embot (version)

data AppState (es :: [*]) (is :: [*]) = AppState
    { _appEnv              :: Env es
    , _appInterceptorState :: HList is
    , _appSequenceNumber   :: Word64
    }

makeLenses ''AppState

main :: IO ()
main = do
    createDirectoryIfMissing False "logs"
    fileLoggerSet   <- newFileLoggerSet 16384 "logs/embot.log"
    stdoutLoggerSet <- newStdoutLoggerSet 16384
    let inEmbotIO :: EmbotIO a -> IO a
        inEmbotIO m = runLoggingT (unEmbotIO m) (emitLogMessage [fileLoggerSet, stdoutLoggerSet])
    inEmbotIO . $logInfo $ "Embot " <> (pack . Show.show $ version) <> " starting"
    inEmbotIO . $logDebug $ "Reading configuration file"
    globalConfig <- inEmbotIO readGlobalConfig
    inEmbotIO . $logInfo $ "Connecting to Slack"
    post "https://slack.com/api/rtm.start" ["token" := apiToken globalConfig]
        >>= asJSON >>= pure . view responseBody
        >>= \ case
            Slack.ResponseNotOk err -> fail . unpack $ "rtm.start failed with " <> err
            Slack.ResponseOk rp -> do
                inEmbotIO . $logDebug $ "rtm.start finished"
                inEmbotIO . $logDebug $ "Initializing environment"
                env <- inEmbotIO $ initializeEnv globalConfig rp
                inEmbotIO . $logDebug $ "Initializing interceptors"
                interceptorWithState <- inEmbotIO $ initializeInterceptors env
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
                    (embotApp inEmbotIO env interceptorWithState)

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

emitLogMessage :: [LoggerSet] -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
emitLogMessage loggers loc src level msg = do
    -- FIXME pick which messages to emit and which to suppress
    now <- getCurrentTime
    let str = "[" <> (toLogStr . pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" $ now) <> "] " <> defaultLogStr loc src level msg
    forM_ loggers $ \ logger ->
        pushLogStr logger str

embotApp :: forall (es :: [*]) (is :: [*]). (forall a. EmbotIO a -> IO a) -> TIP es -> InterceptorWithState es is -> WS.ClientApp ()
embotApp inEmbotIO initialEnv (interceptor, initialState) conn = do
    inEmbotIO . $logInfo $ "RTM connected"
    inEmbotIO . flip evalStateT (AppState initialEnv initialState 1) . forever $ do
        msg <- liftIO $ WS.receiveData conn
        $(logOther "Trace") $ DTE.decodeUtf8 msg
        case Aeson.eitherDecode' (LBS.fromStrict msg) of
            Left failure -> $logWarn $ "failed to parse: " <> pack failure
            Right slackEvent -> do
                $(logOther "Trace") $ " => " <> show (slackEvent :: Slack.RtmEvent)
                process slackEvent
  where
    handleEvent :: Event -> StateT (AppState es is) EmbotIO ()
    handleEvent event = do
        state <- use appInterceptorState
        env <- use appEnv
        (InterceptorState env' state', actions) <- lift $ execRWST (interceptor event) () (InterceptorState env state)
        appInterceptorState .= state'
        appEnv .= env'
        forM_ actions $ \ case
            SendMessage chat message -> do
                seqnum <- use appSequenceNumber
                liftIO $ WS.sendTextData conn (Aeson.encode $ Slack.RtmSendMessage seqnum chat message)
                appSequenceNumber += 1

    process :: Slack.RtmEvent -> StateT (AppState es is) EmbotIO ()
    process (Slack.RtmMessage message) = handleEvent . Event False . ReceivedMessage $ message
    process _ = pure ()

