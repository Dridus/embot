module Main where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens ((^.), (&), (.~), (%~), view)
import           Control.Monad ((<=<), (>>=), fail, forever)
import           Control.Monad.Trans.Reader ()
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bool (Bool(True, False))
import qualified Data.ByteString.Lazy as LBS
import           Data.Either (Either(Left, Right))
import           Data.Function (($), (.), flip)
import           Data.Functor (fmap)
import           Data.List (head)
import           Data.Maybe (Maybe(Just, Nothing))
import           Data.Monoid ((<>))
import           Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as DTE
import           Data.Text.IO (putStrLn)
import           Prelude (Int, Integer, IO, String)
import           Network.URI (parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import qualified OpenSSL                   as SSL
import qualified OpenSSL.Session           as SSL
import qualified Network.Socket            as S
import           Network.Wreq (FormParam((:=)), post, asJSON, responseBody)
import qualified System.IO.Streams         as Streams
import qualified System.IO.Streams.SSL     as Streams
import           Text.Read (read)
import qualified Text.Show as Show
import           Text.Show.Text (show)

import qualified Embot.SlackAPI as Slack
import qualified Embot.Env as Env

main :: IO ()
main = do
    globalConfig <- Env.readGlobalConfig
    let env = Env.Env globalConfig
    post "https://slack.com/api/rtm.start" ["token" := globalConfig ^. Env.apiToken]
        >>= asJSON >>= pure . view responseBody
        >>= \ case
            Slack.ResponseNotOk err -> fail . unpack $ "rtm.start failed with " <> err
            Slack.ResponseOk rp -> do
                putStrLn $ "got " <> show rp
                let url = rp ^. Slack.rtmStartUrl
                uri <- case parseURI (unpack url) of
                    Just u -> pure u
                    Nothing  -> fail . unpack $ "failed to parse websocket URI " <> url
                uriAuth <- case uriAuthority uri of
                    Just ua -> pure ua
                    Nothing -> fail . unpack $ "no URI authority in " <> url
                wss (uriRegName uriAuth) (case uriPort uriAuth of { "" -> 443; other -> read other }) (uriPath uri) app

wss :: String -> Int -> String -> WS.ClientApp a -> IO a
wss host port path app = SSL.withOpenSSL $ do
    ctx <- SSL.context
    is  <- S.getAddrInfo Nothing (Just host) (Just $ Show.show port)
    let a = S.addrAddress $ head is
        f = S.addrFamily $ head is
    s <- S.socket f S.Stream S.defaultProtocol
    S.connect s a
    ssl <- SSL.connection ctx s
    SSL.connect ssl
    (i,o) <- Streams.sslToStreams ssl
    stream <- WS.makeStream (Streams.read i) (\ lbs -> Streams.write (LBS.toStrict <$> lbs) o)
    WS.runClientWithStream stream host path WS.defaultConnectionOptions [] app

app :: WS.ClientApp ()
app conn = forever $ do
    msg <- WS.receiveData conn
    putStrLn $ DTE.decodeUtf8 msg
    case Aeson.eitherDecode' (LBS.fromStrict msg) of
        Left failure -> putStrLn $ "failed to parse: " <> pack failure
        Right event -> putStrLn $ " => " <> show (event :: Slack.RtmEvent)





