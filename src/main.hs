module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Lens ((^.), (&), (.~), (%~), view)
import Control.Lens.TH (makeLenses)
import Control.Monad ((>>=), fail, forever)
import Control.Monad.Trans.Reader ()
import Data.Aeson ((.:), (.:?), (.!=), Value(Object), Object, FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject)
import Data.Bool (Bool(True, False))
import qualified Data.ByteString.Lazy as LBS
import Data.Function (($), (.), flip)
import Data.Functor (fmap)
import Data.List (head)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Prelude (Int, Integer, IO, String)
import Network.URI (parseURI, uriAuthority, uriPath, uriRegName, uriPort)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified OpenSSL               as SSL
import qualified OpenSSL.Session       as SSL
import qualified Network.Socket as S
import Network.Wreq (FormParam((:=)), post, asJSON, responseBody)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.SSL as Streams
import Text.Read (read)
import Text.Show (show)

data SlackRp a = RpNotOk !Text | RpOk a

data RtmStartRq = RtmStartRq { rtmStartToken :: Text }

data RtmStartRp = RtmStartRp
    { _rtmStartUrl      :: Text
    , _rtmStartSelf     :: SlackSelf
    , _rtmStartTeam     :: SlackTeam
    , _rtmStartUsers    :: [SlackUser]
    , _rtmStartChannels :: [SlackChannel]
    , _rtmStartGroups   :: [SlackGroup]
    , _rtmStartIms      :: [SlackIM]
    , _rtmStartBots     :: [SlackBot]
    }

data SlackSelf = SlackSelf
    { _selfId             :: Text
    , _selfName           :: Text
    , _selfPrefs          :: Object
    , _selfCreated        :: Integer
    , _selfManualPresence :: SlackPresence
    }

data SlackPresence = SlackPresenceActive | SlackPresenceAway

data SlackTeam = SlackTeam
    { _teamId                :: Text
    , _teamName              :: Text
    , _teamEmailDomain       :: Maybe Text
    , _teamDomain            :: Text
    , _teamMsgEditWindowMins :: Maybe Int
    , _teamOverStorageLimit  :: Bool
    , _teamPrefs             :: Object
    }

data SlackUser = SlackUser
    { _userId                :: Text
    , _userName              :: Text
    , _userDeleted           :: Bool
    , _userColor             :: Text
    , _userProfile           :: SlackProfile
    , _userIsAdmin           :: Bool
    , _userIsOwner           :: Bool
    , _userIsPrimaryOwner    :: Bool
    , _userIsRestricted      :: Bool
    , _userIsUltraRestricted :: Bool
    , _userHasFiles          :: Bool
    }

data SlackProfile = SlackProfile
    { _profileFirstName :: Maybe Text
    , _profileLastName  :: Maybe Text
    , _profileRealName  :: Maybe Text
    , _profileEmail     :: Maybe Text
    , _profileSkype     :: Maybe Text
    , _profilePhone     :: Maybe Text
    }

data SlackChannel = SlackChannel
    { _channelId          :: Text
    , _channelName        :: Text
    , _channelCreated     :: Integer
    , _channelCreator     :: Text
    , _channelIsArchived  :: Bool
    , _channelIsGeneral   :: Bool
    , _channelMembers     :: [Text]
    , _channelTopic       :: Maybe (SlackTracked Text)
    , _channelPurpose     :: Maybe (SlackTracked Text)
    , _channelIsMember    :: Bool
    , _channelLastRead    :: Maybe Text
    , _channelLatest      :: Maybe SlackMessage
    , _channelUnreadCount :: Maybe Int
    }

data SlackGroup = SlackGroup
    { _groupId          :: Text
    , _groupName        :: Text
    , _groupCreated     :: Integer
    , _groupCreator     :: Text
    , _groupIsArchived  :: Bool
    , _groupMembers     :: [Text]
    , _groupTopic       :: Maybe (SlackTracked Text)
    , _groupPurpose     :: Maybe (SlackTracked Text)
    , _groupIsOpen      :: Bool
    , _groupLastRead    :: Maybe Text
    , _groupLatest      :: Maybe SlackMessage
    , _groupUnreadCount :: Maybe Int
    }

data SlackIM = SlackIM
    { _imId            :: Text
    , _imUser          :: Text
    , _imCreated       :: Integer
    , _imIsUserDeleted :: Bool
    , _imIsOpen        :: Bool
    , _imLastRead      :: Maybe Text
    , _imLatest        :: Maybe SlackMessage
    , _imUnreadCount   :: Maybe Int
    }

data SlackBot = SlackBot Object

data SlackMessage = SlackMessage
    { _messageChannel     :: Maybe Text
    , _messageUser        :: Text
    , _messageSubtype     :: Maybe SlackMessageSubtype
    , _messageText        :: Text
    , _messageTS          :: Text
    , _messageEdited      :: Maybe SlackMessageEdited
    , _messageDeletedTS   :: Maybe Text
    , _messageEventTS     :: Maybe Text
    , _messageHidden      :: Bool
    , _messageAttachments :: [SlackAttachment]
    }

data SlackMessageSubtype = SlackBotMessage | SlackMeMessage | SlackMessageChanged | SlackMessageDeleted
    | SlackChannelJoin | SlackChannelLeave | SlackChannelTopic | SlackChannelPurpose | SlackChannelName
    | SlackChannelArchive | SlackChannelUnarchive | SlackGroupJoin | SlackGroupLeave | SlackGroupTopic
    | SlackGroupPurpose | SlackGroupName | SlackGroupArchive | SlackGroupUnarchive | SlackFileShare
    | SlackFileComment | SlackFileMention

data SlackMessageEdited = SlackMessageEdited
    { _messageEditedUser :: Text
    , _messageEditedTS :: Text
    }

data SlackAttachment = SlackAttachment
    { _attachmentFallback :: Text
    , _attachmentColor :: Maybe Text
    , _attachmentPretext :: Maybe Text
    , _attachmentAuthorName :: Maybe Text
    , _attachmentAuthorLink :: Maybe Text
    , _attachmentAuthorIcon :: Maybe Text
    , _attachmentTitle :: Maybe Text
    , _attachmentTitleLink :: Maybe Text
    , _attachmentText :: Maybe Text
    , _attachmentFields :: [SlackAttachmentField]
    }

data SlackAttachmentField = SlackAttachmentField
    { _fieldTitle :: Text
    , _fieldValue :: Text
    , _fieldShort :: Bool
    }

data SlackTracked a = SlackTracked
    { _trackedValue :: a
    , _trackedCreator :: Text
    , _trackedLastSet :: Integer
    }

makeLenses ''RtmStartRq
makeLenses ''RtmStartRp
makeLenses ''SlackSelf
makeLenses ''SlackTeam
makeLenses ''SlackUser
makeLenses ''SlackProfile
makeLenses ''SlackChannel
makeLenses ''SlackGroup
makeLenses ''SlackIM
makeLenses ''SlackMessage
makeLenses ''SlackMessageEdited
makeLenses ''SlackAttachment
makeLenses ''SlackAttachmentField
makeLenses ''SlackTracked

instance ToJSON RtmStartRq where
    toJSON (RtmStartRq { .. }) = object
        [ ("token", toJSON rtmStartToken) ]

instance FromJSON a => FromJSON (SlackRp a) where
    parseJSON = withObject "slack reply" $ \ o ->
      o .: "ok" >>= \ case
        True -> RpOk <$> parseJSON (Object o)
        False -> RpNotOk <$> o .:? "error" .!= "unknown error"

instance FromJSON RtmStartRp where
    parseJSON = withObject "rtm.start reply" $ \ o -> RtmStartRp
        <$> o .: "url"
        <*> o .: "self"
        <*> o .: "team"
        <*> o .: "users"
        <*> o .: "channels"
        <*> o .: "groups"
        <*> o .: "ims"
        <*> o .: "bots"

instance FromJSON SlackSelf where
    parseJSON = withObject "self object" $ \ o -> SlackSelf
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "prefs"
        <*> o .: "created"
        <*> o .: "manual_presence"

instance FromJSON SlackPresence where
    parseJSON = withText "presence value" $ \ case
        "active" -> pure SlackPresenceActive
        "away"   -> pure SlackPresenceAway
        other    -> fail . unpack $ "unknown presence value " <> other

instance FromJSON SlackTeam where
    parseJSON = withObject "team object" $ \ o -> SlackTeam
        <$> o .: "id"
        <*> o .: "name"
        <*> (o .:? "email_domain" >>= \ case
                Just "" -> pure Nothing
                Just s  -> pure $ Just s
                Nothing -> pure Nothing)
        <*> o .: "domain"
        <*> (o .:? "msg_edit_window_mins" >>= \ case
                Just (-1) -> pure Nothing
                Just i    -> pure $ Just i
                Nothing   -> pure Nothing)
        <*> o .: "over_storage_limit"
        <*> o .: "prefs"

instance FromJSON SlackUser where
    parseJSON = withObject "user object" $ \ o -> SlackUser
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "deleted"
        <*> o .: "color"
        <*> o .: "profile"
        <*> o .: "is_admin"
        <*> o .: "is_owner"
        <*> o .: "is_primary_owner"
        <*> o .: "is_restricted"
        <*> o .: "is_ultra_restricted"
        <*> o .:? "has_files" .!= False

instance FromJSON SlackProfile where
    parseJSON = withObject "user profile object" $ \ o -> SlackProfile
        <$> o .:? "first_name"
        <*> o .:? "last_name"
        <*> o .:? "real_name"
        <*> o .:? "email"
        <*> o .:? "skype"
        <*> o .:? "phone"

instance FromJSON SlackChannel where
    parseJSON = withObject "channel object" $ \ o -> SlackChannel
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "created"
        <*> o .: "creator"
        <*> o .: "is_archived"
        <*> o .: "is_general"
        <*> o .:? "members" .!= []
        <*> o .:? "topic"
        <*> o .:? "purpose"
        <*> o .:? "is_member" .!= False
        <*> o .:? "last_read"
        <*> o .:? "latest"
        <*> o .:? "unread_count"

instance FromJSON SlackGroup where
    parseJSON = withObject "group object" $ \ o -> SlackGroup
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "created"
        <*> o .: "creator"
        <*> o .: "is_archived"
        <*> o .:? "members" .!= []
        <*> o .:? "topic"
        <*> o .:? "purpose"
        <*> o .:? "is_open" .!= False
        <*> o .:? "last_read"
        <*> o .:? "latest"
        <*> o .:? "unread_count"

instance FromJSON SlackIM where
    parseJSON = withObject "im object" $ \ o -> SlackIM
        <$> o .: "id"
        <*> o .: "user"
        <*> o .: "created"
        <*> o .:? "is_user_deleted" .!= False
        <*> o .:? "is_open" .!= False
        <*> o .:? "last_read"
        <*> o .:? "latest"
        <*> o .:? "unread_count"

instance FromJSON SlackBot where
    parseJSON = withObject "bot object" (pure . SlackBot)

instance FromJSON a => FromJSON (SlackTracked a) where
    parseJSON = withObject "tracked value object" $ \ o -> SlackTracked
        <$> o .: "value"
        <*> o .: "creator"
        <*> o .: "last_set"

instance FromJSON SlackMessage where
    parseJSON = withObject "message object" $ \ o -> SlackMessage
        <$> o .:? "channel"
        <*> o .: "user"
        <*> o .:? "subtype"
        <*> o .: "text"
        <*> o .: "ts"
        <*> o .:? "edited"
        <*> o .:? "deleted_ts"
        <*> o .:? "event_ts"
        <*> o .:? "hidden" .!= False
        <*> o .:? "attachments" .!= []

instance FromJSON SlackMessageSubtype where
    parseJSON = withText "message subtype" $ \ case
        "botmessage"       -> pure SlackBotMessage
        "memessage"        -> pure SlackMeMessage
        "messagechanged"   -> pure SlackMessageChanged
        "messagedeleted"   -> pure SlackMessageDeleted
        "channeljoin"      -> pure SlackChannelJoin
        "channelleave"     -> pure SlackChannelLeave
        "channeltopic"     -> pure SlackChannelTopic
        "channelpurpose"   -> pure SlackChannelPurpose
        "channelname"      -> pure SlackChannelName
        "channelarchive"   -> pure SlackChannelArchive
        "channelunarchive" -> pure SlackChannelUnarchive
        "groupjoin"        -> pure SlackGroupJoin
        "groupleave"       -> pure SlackGroupLeave
        "grouptopic"       -> pure SlackGroupTopic
        "grouppurpose"     -> pure SlackGroupPurpose
        "groupname"        -> pure SlackGroupName
        "grouparchive"     -> pure SlackGroupArchive
        "groupunarchive"   -> pure SlackGroupUnarchive
        "fileshare"        -> pure SlackFileShare
        "filecomment"      -> pure SlackFileComment
        "filemention"      -> pure SlackFileMention
        other              -> fail . unpack $ "unknown message subtype " <> other

instance FromJSON SlackMessageEdited where
    parseJSON = withObject "message edited object" $ \ o -> SlackMessageEdited
        <$> o .: "user"
        <*> o .: "ts"

instance FromJSON SlackAttachment where
    parseJSON = withObject "attachment object" $ \ o -> SlackAttachment
        <$> o .: "fallback"
        <*> o .:? "color"
        <*> o .:? "pretext"
        <*> o .:? "author_name"
        <*> o .:? "author_link"
        <*> o .:? "author_icon"
        <*> o .:? "title"
        <*> o .:? "title_link"
        <*> o .:? "text"
        <*> o .:? "fields" .!= []

instance FromJSON SlackAttachmentField where
    parseJSON = withObject "attachment field object" $ \ o -> SlackAttachmentField
        <$> o .: "title"
        <*> o .: "value"
        <*> o .: "short"

apiToken :: Text
apiToken = "xoxb-3212154165-oIlmPOvmFrSIDAFYwnLXjPq8"
main :: IO ()
main = post "https://slack.com/api/rtm.start" ["token" := apiToken] >>= asJSON >>= pure . view responseBody >>= \ case
    RpNotOk err -> fail . unpack $ "rtm.start failed with " <> err
    RpOk rp -> do
        let url = rp ^. rtmStartUrl
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
    is  <- S.getAddrInfo Nothing (Just host) (Just $ show port)
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
    putStrLn msg



