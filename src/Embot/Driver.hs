module Embot.Driver
  ( embot
  ) where

import           ClassyPrelude
import qualified Config.Dyre as Dyre
import           Control.Concurrent (forkFinally, forkIO, threadDelay)
import           Control.Lens ((+=), (.=), set, use, view, to)
import           Control.Lens.TH (makeLenses
                                 )
import           Control.Monad.Logger (MonadLogger, logDebug, logError, logInfo, logOther, logWarn, runLoggingT)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Wire (Session, Timed, clockSession_, stepSession, stepWire)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as DTE
import           Data.Time.Clock (NominalDiffTime)
import qualified Network.Socket as Socket
import           Network.URI (parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WSStream
import           Network.Wreq (FormParam((:=)), post, asJSON, responseBody)
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSLSession
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.SSL as StreamsSSL
import qualified Text.Read as StringRead
import qualified Text.Show as StringShow
import           TextShow (showt)

import           Embot.Action (Action(SendMessage))
import qualified Embot.Slack as Slack
import           Embot.Types ( EmbotConfig, apiToken, configurationError, setupLogging, withEnvironment
                             , EmbotLogic, LoggingIO(unLoggingIO), MinimalEmbotMonad )

import Paths_embot (version)

data EmbotState m = EmbotState
  { _stateWire           :: EmbotLogic m
  , _stateSession        :: Session m (Timed NominalDiffTime ())
  , _stateSequenceNumber :: Word64 }
makeLenses ''EmbotState

embot :: (MinimalEmbotMonad m) => EmbotConfig m -> IO ()
embot = Dyre.wrapMain $ Dyre.defaultParams
  { Dyre.projectName = "Embot"
  , Dyre.realMain    = embotMain
  , Dyre.showError   = flip $ set configurationError . Just . pack }

embotMain :: (MinimalEmbotMonad m) => EmbotConfig m -> IO ()
embotMain config = do
  loggerFunc <- view setupLogging config
  let inLoggingIO :: LoggingIO a -> IO a
      inLoggingIO = flip runLoggingT loggerFunc . unLoggingIO
  inLoggingIO . $logInfo $ "Embot " <> showt version <> " starting"
  forM_ (view configurationError config) $ \ err ->
    inLoggingIO . $logError $ "Error occurred during configuration reload: " <> err
  inLoggingIO . $logInfo $ "Connecting to Slack"
  post "https://slack.com/api/rtm.start" ["token" := view apiToken config]
    >>= asJSON >>= pure . view responseBody
    >>= \ case
      Slack.ResponseNotOk err -> fail . unpack $ "rtm.start failed with " <> err
      Slack.ResponseOk rp -> do
        inLoggingIO . $logDebug
          $  "Connected to Slack as " ++ view (Slack.rtmStartSelf . to (Slack.idedName Slack.selfName Slack.selfID)) rp
          ++ " into team " ++ view (Slack.rtmStartTeam . to (Slack.idedName Slack.teamName Slack.teamID)) rp
          ++ " with " ++ showt (length $ view Slack.rtmStartUsers rp) ++ " users, "
          ++ showt (length $ view Slack.rtmStartChannels rp) ++ " channels, "
          ++ showt (length $ view Slack.rtmStartGroups rp) ++ " groups, "
          ++ showt (length $ view Slack.rtmStartIMs rp) ++ " IMs"

        let url = view Slack.rtmStartUrl rp
        uri <- case parseURI (unpack url) of
          Just u -> pure u
          Nothing  -> fail . unpack $ "failed to parse websocket URI " <> url
        uriAuth <- case uriAuthority uri of
          Just ua -> pure ua
          Nothing -> fail . unpack $ "no URI authority in " <> url

        wss
          (uriRegName uriAuth)
          (case uriPort uriAuth of { "" -> 443; other -> StringRead.read other })
          (uriPath uri)
          (embotApp inLoggingIO config rp)

wss :: String -> Int -> String -> WS.ClientApp a -> IO a
wss host port path app = SSL.withOpenSSL $ do
  ctx <- SSLSession.context
  addrInfoList <- Socket.getAddrInfo Nothing (Just host) (Just . StringShow.show $ port)
  let addrInfo = case addrInfoList of
          ai : _ -> ai
          _ -> error "Socket.getAddrInfo lies about its behavior and returned the empty list"
      a = Socket.addrAddress addrInfo
      f = Socket.addrFamily addrInfo
  s <- Socket.socket f Socket.Stream Socket.defaultProtocol
  Socket.connect s a
  ssl <- SSLSession.connection ctx s
  SSLSession.connect ssl
  (i,o) <- StreamsSSL.sslToStreams ssl
  stream <- WSStream.makeStream (Streams.read i) (\ lbs -> Streams.write (LBS.toStrict <$> lbs) o)
  WS.runClientWithStream stream host path WS.defaultConnectionOptions [] app

embotApp :: (Applicative m, Monad m, MonadIO m, MonadLogger m) => (forall a. LoggingIO a -> IO a) -> EmbotConfig m -> Slack.RtmStartRp -> WS.ClientApp ()
embotApp inLoggingIO config rtmStartRp conn = inLoggingIO $ do
  $logInfo $ "RTM connected"
  $logDebug $ "entering configured environment"
  view withEnvironment config rtmStartRp $ \ logic -> do
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
            $(logOther "Trace") $ " => " <> showt (slackEvent :: Slack.RtmEvent)
            liftIO . writeChan chan . Just $ slackEvent )
      (\ result -> inLoggingIO $ case result of
          Left exception -> do
            $logError $ "reader thread failed: " <> showt exception
          Right () -> do
            $logWarn $ "reader thread ended" )

    idlerThreadId <- liftIO . forkIO . forever $ do
      writeChan chan Nothing
      threadDelay 1000000

    let _ = readerThreadId
        _ = idlerThreadId

    let initialSession = clockSession_
        initialState = EmbotState
          { _stateWire = logic
          , _stateSession = initialSession
          , _stateSequenceNumber = 1 }

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
