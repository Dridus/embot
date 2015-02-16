{-# OPTIONS_GHC -fno-warn-orphans #-}
module Embot.SlackAPI where

import           ClassyPrelude (Bool(True, False), Int, (&&), (||), concatMap, map, not)
import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens.TH (makeLenses, makePrisms)
import           Control.Monad ((>>=), fail)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(Object, String), Object, FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText)
import           Data.Aeson.Types (Parser)
import           Data.Eq (Eq)
import           Data.Function (($), (.), flip)
import qualified Data.HashMap.Strict as HM
import           Data.Int (Int32)
import           Data.Maybe (Maybe(Just, Nothing), maybeToList)
import           Data.Monoid ((<>))
import           Data.Ord (Ord)
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger)
import           Data.Text (Text, unpack, isPrefixOf, stripPrefix)
import           Data.Word (Word32, Word64)
import           Text.Show.Text (FromStringShow(FromStringShow), Show(showbPrec), show)
import           Text.Show.Text.TH (deriveShow)

asText :: Text -> Parser Text
asText = pure

-- powerful orphan instance time!
instance Show Value where
    showbPrec prec = showbPrec prec . FromStringShow
instance (Show k, Show v) => Show (HM.HashMap k v) where
    showbPrec prec = ("HM.fromList " <>) . showbPrec prec . HM.toList

newtype TS = TS { unTS :: Text } deriving (Eq, Ord)
instance FromJSON TS where
    parseJSON = withText "timestamp" $ pure . TS
deriveShow ''TS

newtype Time = Time { unTime :: Word32 } deriving (Eq, Ord)
instance FromJSON Time where
    parseJSON = withScientific "time" $ \ s ->
        case toBoundedInteger s of
            Just w32 -> pure (Time w32)
            Nothing  -> fail . unpack $ "out of bound unix time " <> show (FromStringShow s)
deriveShow ''Time

newtype ID a = ID { unID :: Text } deriving (Eq)
instance FromJSON (ID a) where
    parseJSON = withText "id" $ pure . ID
instance ToJSON (ID a) where
    toJSON = String . unID
deriveShow ''ID

data Response a = ResponseNotOk !Text | ResponseOk a

data RtmStartRequest = RtmStartRequest { rtmStartToken :: Text }

data RtmStartRp = RtmStartRp
    { _rtmStartUrl      :: Text
    , _rtmStartSelf     :: Self
    , _rtmStartTeam     :: Team
    , _rtmStartUsers    :: [User]
    , _rtmStartChannels :: [Channel]
    , _rtmStartGroups   :: [Group]
    , _rtmStartIms      :: [IM]
    , _rtmStartBots     :: [Bot]
    }

data Self = Self
    { _selfId             :: ID User
    , _selfName           :: Text
    , _selfPrefs          :: Object
    , _selfCreated        :: Time
    , _selfManualPresence :: Presence
    }

data Presence = PresenceActive | PresenceAway

data Team = Team
    { _teamId                :: ID Team
    , _teamName              :: Text
    , _teamEmailDomain       :: Maybe Text
    , _teamDomain            :: Text
    , _teamMsgEditWindowMins :: Maybe Int
    , _teamOverStorageLimit  :: Bool
    , _teamPrefs             :: Object
    }

data User = User
    { _userId                :: ID User
    , _userName              :: Text
    , _userDeleted           :: Bool
    , _userColor             :: Text
    , _userProfile           :: Profile
    , _userIsAdmin           :: Bool
    , _userIsOwner           :: Bool
    , _userIsPrimaryOwner    :: Bool
    , _userIsRestricted      :: Bool
    , _userIsUltraRestricted :: Bool
    , _userHasFiles          :: Bool
    }

data Profile = Profile
    { _profileFirstName :: Maybe Text
    , _profileLastName  :: Maybe Text
    , _profileRealName  :: Maybe Text
    , _profileEmail     :: Maybe Text
    , _profileSkype     :: Maybe Text
    , _profilePhone     :: Maybe Text
    }

data Channel = Channel
    { _channelId          :: ID Channel
    , _channelName        :: Text
    , _channelCreated     :: Time
    , _channelCreator     :: ID User
    , _channelIsArchived  :: Bool
    , _channelIsGeneral   :: Bool
    , _channelMembers     :: [ID User]
    , _channelTopic       :: Maybe (SlackTracked Text)
    , _channelPurpose     :: Maybe (SlackTracked Text)
    , _channelIsMember    :: Bool
    , _channelLastRead    :: Maybe TS
    , _channelLatest      :: Maybe Message
    , _channelUnreadCount :: Maybe Int
    }

data Group = Group
    { _groupId          :: ID Group
    , _groupName        :: Text
    , _groupCreated     :: Time
    , _groupCreator     :: ID User
    , _groupIsArchived  :: Bool
    , _groupMembers     :: [ID User]
    , _groupTopic       :: Maybe (SlackTracked Text)
    , _groupPurpose     :: Maybe (SlackTracked Text)
    , _groupIsOpen      :: Bool
    , _groupLastRead    :: Maybe TS
    , _groupLatest      :: Maybe Message
    , _groupUnreadCount :: Maybe Int
    }

data IM = IM
    { _imId            :: ID IM
    , _imUser          :: ID User
    , _imCreated       :: Time
    , _imIsUserDeleted :: Bool
    , _imIsOpen        :: Bool
    , _imLastRead      :: Maybe TS
    , _imLatest        :: Maybe Message
    , _imUnreadCount   :: Maybe Int
    }

data Bot = Bot
    { _botId    :: ID Bot
    , _botName  :: Text
    , _botIcons :: HM.HashMap Text Text
    }

data Conversation

data Message = Message
    { _messageChannel     :: Maybe (ID Conversation)
    , _messageUser        :: ID User
    , _messageSubtype     :: Maybe MessageSubtype
    , _messageText        :: Text
    , _messageTS          :: TS
    , _messageEdited      :: Maybe MessageEdited
    , _messageDeletedTS   :: Maybe TS
    , _messageEventTS     :: Maybe TS
    , _messageHidden      :: Bool
    , _messageAttachments :: [Attachment]
    , _messageInviter     :: Maybe (ID User)
    }

data MessageSubtype
    = BotMS | MeMS | ChangedMS | DeletedMS
    | ChannelJoinMS | ChannelLeaveMS | ChannelTopicMS | ChannelPurposeMS | ChannelNameMS | ChannelArchiveMS | ChannelUnarchiveMS
    | GroupJoinMS   | GroupLeaveMS   | GroupTopicMS   | GroupPurposeMS   | GroupNameMS   | GroupArchiveMS   | GroupUnarchiveMS
    | FileShareMS | FileCommentMS | FileMentionMS

data MessageEdited = MessageEdited
    { _messageEditedUser :: ID User
    , _messageEditedTS   :: TS
    }

data Attachment = Attachment
    { _attachmentFallback   :: Text
    , _attachmentColor      :: Maybe Text
    , _attachmentPretext    :: Maybe Text
    , _attachmentAuthorName :: Maybe Text
    , _attachmentAuthorLink :: Maybe Text
    , _attachmentAuthorIcon :: Maybe Text
    , _attachmentTitle      :: Maybe Text
    , _attachmentTitleLink  :: Maybe Text
    , _attachmentText       :: Maybe Text
    , _attachmentFields     :: [AttachmentField]
    }

data AttachmentField = AttachmentField
    { _fieldTitle :: Text
    , _fieldValue :: Text
    , _fieldShort :: Bool
    }

data SlackTracked a = SlackTracked
    { _trackedValue   :: a
    , _trackedCreator :: ID User
    , _trackedLastSet :: Time
    }

data File = File
    { _fileId                 :: ID File
    , _fileCreated            :: Time
    , _fileTimestamp          :: Time
    , _fileName               :: Text
    , _fileTitle              :: Text
    , _fileMimeType           :: Text
    , _fileFileType           :: Text
    , _filePrettyType         :: Text
    , _fileUser               :: ID User
    , _fileMode               :: FileMode
    , _fileEditable           :: Bool
    , _fileIsExternal         :: Bool
    , _fileExternalType       :: Text
    , _fileSize               :: Word64
    , _fileURL                :: Text
    , _fileURLDownload        :: Text
    , _fileURLPrivate         :: Text
    , _fileURLPrivateDownload :: Text
    , _fileThumb              :: HM.HashMap Text Text
    , _filePermalink          :: Text
    , _fileEditLink           :: Text
    , _filePreview            :: Text
    , _filePreviewHighlight   :: Text
    , _fileLines              :: Int
    , _fileLinesMore          :: Int
    , _fileIsPublic           :: Bool
    , _filePublicURLShared    :: Bool
    , _fileChannels           :: [ID Channel]
    , _fileGroups             :: [ID Group]
    , _fileIMs                :: [ID IM]
    , _fileInitialComment     :: Maybe Message
    , _fileNumStars           :: Int
    , _fileIsStarred          :: Bool
    }

data FileMode
    = FileHosted
    | FileExternal
    | FileSnippet
    | FilePost

data FileComment = FileComment
    { _fileCommentId        :: ID FileComment
    , _fileCommentTimestamp :: Time
    , _fileCommentUser      :: ID User
    , _fileCommentComment   :: Text
    }

data RtmEvent
    = RtmHello
    | RtmReplyOk Word64 (Maybe TS) (Maybe Text)
    | RtmReplyNotOk Word64 Int32 Text
    | RtmMessage Message
    | RtmChannelMarked (ConversationMarked Channel)
    | RtmChannelCreated Channel
    | RtmChannelJoined Channel
    | RtmChannelLeft Channel
    | RtmChannelDeleted (ID Channel)
    | RtmChannelRenamed Channel
    | RtmChannelArchive (ConversationUser Channel)
    | RtmChannelUnarchive (ConversationUser Channel)
    | RtmChannelHistoryChanged (ConversationHistoryChanged Channel)
    | RtmImCreated ImCreated
    | RtmImOpen (ConversationUser IM)
    | RtmImClose (ConversationUser IM)
    | RtmImMarked (ConversationMarked IM)
    | RtmImHistoryChanged (ConversationHistoryChanged IM)
    | RtmGroupJoined Group
    | RtmGroupLeft Group
    | RtmGroupOpen (ConversationUser Group)
    | RtmGroupClose (ConversationUser Group)
    | RtmGroupArchive (ID Group)
    | RtmGroupUnarchive (ID Group)
    | RtmGroupRename Group
    | RtmGroupMarked (ConversationMarked Group)
    | RtmGroupHistoryChanged (ConversationHistoryChanged Group)
    | RtmFileCreated File
    | RtmFileShared File
    | RtmFileUnshared File
    | RtmFilePublic File
    | RtmFilePrivate (ID File)
    | RtmFileChange File
    | RtmFileDeleted FileDeleted
    | RtmFileCommentAdded FileCommentUpdated
    | RtmFileCommentEdited FileCommentUpdated
    | RtmFileCommentDeleted FileCommentDeleted
    | RtmPresenceChange PresenceChange
    | RtmManualPresenceChange Presence
    | RtmPrefChange PrefChange
    | RtmUserChange User
    | RtmUserTyping UserTyping
    | RtmTeamJoin User
    | RtmStarAdded Star
    | RtmStarRemoved Star
    | RtmEmojiChanged TS
    | RtmCommandsChanged TS
    | RtmTeamPrefChange PrefChange
    | RtmTeamRename Text
    | RtmTeamDomainChange TeamDomainChange
    | RtmEmailDomainChanged EmailDomainChanged
    | RtmBotAdded Bot
    | RtmBotChanged Bot
    | RtmAccountsChanged

data ConversationMarked a = ConversationMarked
    { _conversationMarkedChannel :: ID a
    , _conversationMarkedTS      :: TS
    }

data ConversationUser a = ConversationUser
    { _conversationUserUser    :: ID User
    , _conversationUserChannel :: ID a
    }

data ConversationHistoryChanged a = ConversationHistoryChanged
    { _conversationHistoryChangedLatest  :: Text
    , _conversationHistoryChangedTS      :: TS
    , _conversationHistoryChangedEventTS :: TS
    }

data ImCreated = ImCreated
    { _imCreatedUser    :: Text
    , _imCreatedChannel :: Channel
    }

data FileDeleted = FileDeleted
    { _fileDeletedFileId  :: Text
    , _fileDeletedEventTS :: Text
    }

data FileCommentUpdated = FileCommentUpdated
    { _fileCommentUpdatedFile    :: File
    , _fileCommentUpdatedComment :: FileComment
    }

data FileCommentDeleted = FileCommentDeleted
    { _fileCommentDeletedFile    :: File
    , _fileCommentDeletedComment :: ID FileComment
    }

data PresenceChange = PresenceChange
    { _presenceChangeUser     :: ID User
    , _presenceChangePresence :: Presence
    }

data PrefChange = PrefChange
    { _prefChangeName  :: Text
    , _prefChangeValue :: Value
    }

data UserTyping = UserTyping
    { _userTypingUser :: ID User
    , _userTypingChannel :: ID Conversation
    }

data Star = Star
    { _starUser :: Text
    , _starItem :: StarItem
    , _starEventTS :: TS
    }

data StarItem
    = StarItemMessage Message
    | StarItemFile File
    | StarItemFileComment File FileComment
    | StarItemChannel (ID Channel)
    | StarItemIM (ID IM)
    | StarItemGroup (ID Group)

data TeamDomainChange = TeamDomainChange
    { _teamDomainChangeUrl    :: Text
    , _teamDomainChangeDomain :: Text
    }

data EmailDomainChanged = EmailDomainChanged
    { _emailDomainChangedEmailDomain :: Text
    , _emailDomainChangedEventTS     :: TS
    }

data RtmSendMessage = RtmSendMessage
    { _sendMessageSeqnum       :: Word64
    , _sendMessageConversation :: ID Conversation
    , _sendMessageText         :: Text
    }

class SlackTyped a where
    isTypedID :: Proxy a -> Text -> Bool
instance SlackTyped Channel where
    isTypedID _ = isPrefixOf "C"
instance SlackTyped File where
    isTypedID _ t = "F" `isPrefixOf` t && not ("Fc" `isPrefixOf` t)
instance SlackTyped FileComment where
    isTypedID _ t = "Fc" `isPrefixOf` t
instance SlackTyped Group where
    isTypedID _ = isPrefixOf "G"
instance SlackTyped Conversation where
     isTypedID _ t
        =  isTypedID (Proxy :: Proxy Channel) t
        || isTypedID (Proxy :: Proxy IM) t
        || isTypedID (Proxy :: Proxy Group) t
instance SlackTyped IM where
    isTypedID _ = isPrefixOf "D"
instance SlackTyped User where
    isTypedID _ = isPrefixOf "U"

asTypedID :: forall a b. SlackTyped b => ID a -> Maybe (ID b)
asTypedID (ID t) =
    if isTypedID (Proxy :: Proxy b) t
        then Just (ID t)
        else Nothing

asChannelID :: ID Conversation -> Maybe (ID Channel)
asChannelID = asTypedID
asGroupID :: ID Conversation -> Maybe (ID Group)
asGroupID = asTypedID
asIMID :: ID Conversation -> Maybe (ID IM)
asIMID = asTypedID

makeLenses ''RtmStartRequest
makeLenses ''RtmStartRp
makeLenses ''Self
makeLenses ''Team
makeLenses ''User
makeLenses ''Profile
makeLenses ''Channel
makeLenses ''Group
makeLenses ''IM
makeLenses ''Bot
makeLenses ''Message
makeLenses ''MessageEdited
makeLenses ''Attachment
makeLenses ''AttachmentField
makeLenses ''SlackTracked
makeLenses ''File
makeLenses ''FileComment
makePrisms ''RtmEvent
makeLenses ''ConversationMarked
makeLenses ''ConversationUser
makeLenses ''ConversationHistoryChanged
makeLenses ''ImCreated
makeLenses ''FileDeleted
makeLenses ''FileCommentUpdated
makeLenses ''FileCommentDeleted
makeLenses ''PresenceChange
makeLenses ''UserTyping
makeLenses ''PrefChange
makeLenses ''Star
makePrisms ''StarItem
makeLenses ''TeamDomainChange
makeLenses ''EmailDomainChanged
makeLenses ''RtmSendMessage

deriveShow ''RtmStartRequest
deriveShow ''RtmStartRp
deriveShow ''Self
deriveShow ''Presence
deriveShow ''Team
deriveShow ''User
deriveShow ''Profile
deriveShow ''Channel
deriveShow ''Group
deriveShow ''IM
deriveShow ''Bot
deriveShow ''Message
deriveShow ''MessageSubtype
deriveShow ''MessageEdited
deriveShow ''Attachment
deriveShow ''AttachmentField
deriveShow ''SlackTracked
deriveShow ''File
deriveShow ''FileMode
deriveShow ''FileComment
deriveShow ''RtmEvent
deriveShow ''ConversationMarked
deriveShow ''ConversationUser
deriveShow ''ConversationHistoryChanged
deriveShow ''ImCreated
deriveShow ''FileDeleted
deriveShow ''FileCommentUpdated
deriveShow ''FileCommentDeleted
deriveShow ''PresenceChange
deriveShow ''UserTyping
deriveShow ''PrefChange
deriveShow ''Star
deriveShow ''StarItem
deriveShow ''TeamDomainChange
deriveShow ''EmailDomainChanged
deriveShow ''RtmSendMessage

instance ToJSON RtmStartRequest where
    toJSON (RtmStartRequest { .. }) = object
        [ ("token", toJSON rtmStartToken) ]

instance FromJSON a => FromJSON (Response a) where
    parseJSON = withObject "slack reply" $ \ o ->
      o .: "ok" >>= \ case
        True -> ResponseOk <$> parseJSON (Object o)
        False -> ResponseNotOk <$> o .:? "error" .!= "unknown error"

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

instance FromJSON Self where
    parseJSON = withObject "self object" $ \ o -> Self
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "prefs"
        <*> o .: "created"
        <*> o .: "manual_presence"

instance FromJSON Presence where
    parseJSON = withText "presence value" $ \ case
        "active" -> pure PresenceActive
        "away"   -> pure PresenceAway
        other    -> fail . unpack $ "unknown presence value " <> other

instance FromJSON Team where
    parseJSON = withObject "team object" $ \ o -> Team
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

instance FromJSON User where
    parseJSON = withObject "user object" $ \ o -> User
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

instance FromJSON Profile where
    parseJSON = withObject "user profile object" $ \ o -> Profile
        <$> o .:? "first_name"
        <*> o .:? "last_name"
        <*> o .:? "real_name"
        <*> o .:? "email"
        <*> o .:? "skype"
        <*> o .:? "phone"

instance FromJSON Channel where
    parseJSON = withObject "channel object" $ \ o -> Channel
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "created"
        <*> o .: "creator"
        <*> o .: "is_archived"
        <*> o .:? "is_general" .!= False
        <*> o .:? "members" .!= []
        <*> o .:? "topic"
        <*> o .:? "purpose"
        <*> o .:? "is_member" .!= False
        <*> o .:? "last_read"
        <*> o .:? "latest"
        <*> o .:? "unread_count"

instance FromJSON Group where
    parseJSON = withObject "group object" $ \ o -> Group
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

instance FromJSON IM where
    parseJSON = withObject "im object" $ \ o -> IM
        <$> o .: "id"
        <*> o .: "user"
        <*> o .: "created"
        <*> o .:? "is_user_deleted" .!= False
        <*> o .:? "is_open" .!= False
        <*> o .:? "last_read"
        <*> o .:? "latest"
        <*> o .:? "unread_count"

instance FromJSON Bot where
    parseJSON = withObject "bot object" $ \ o -> Bot
        <$> o .: "id"
        <*> o .: "name"
        <*> o .:? "icons" .!= HM.empty

instance FromJSON a => FromJSON (SlackTracked a) where
    parseJSON = withObject "tracked value object" $ \ o -> SlackTracked
        <$> o .: "value"
        <*> o .: "creator"
        <*> o .: "last_set"

instance FromJSON Message where
    parseJSON = withObject "message object" $ \ o -> Message
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
        <*> o .:? "inviter"

instance FromJSON MessageSubtype where
    parseJSON = withText "message subtype" $ \ case
        "bot_message"       -> pure BotMS
        "me_message"        -> pure MeMS
        "message_changed"   -> pure ChangedMS
        "message_deleted"   -> pure DeletedMS
        "channel_join"      -> pure ChannelJoinMS
        "channel_leave"     -> pure ChannelLeaveMS
        "channel_topic"     -> pure ChannelTopicMS
        "channel_purpose"   -> pure ChannelPurposeMS
        "channel_name"      -> pure ChannelNameMS
        "channel_archive"   -> pure ChannelArchiveMS
        "channel_unarchive" -> pure ChannelUnarchiveMS
        "group_join"        -> pure GroupJoinMS
        "group_leave"       -> pure GroupLeaveMS
        "group_topic"       -> pure GroupTopicMS
        "group_purpose"     -> pure GroupPurposeMS
        "group_name"        -> pure GroupNameMS
        "group_archive"     -> pure GroupArchiveMS
        "group_unarchive"   -> pure GroupUnarchiveMS
        "file_share"        -> pure FileShareMS
        "file_comment"      -> pure FileCommentMS
        "file_mention"      -> pure FileMentionMS
        other               -> fail . unpack $ "unknown message subtype " <> other

instance FromJSON MessageEdited where
    parseJSON = withObject "message edited object" $ \ o -> MessageEdited
        <$> o .: "user"
        <*> o .: "ts"

instance FromJSON Attachment where
    parseJSON = withObject "attachment object" $ \ o -> Attachment
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

instance FromJSON AttachmentField where
    parseJSON = withObject "attachment field object" $ \ o -> AttachmentField
        <$> o .: "title"
        <*> o .: "value"
        <*> o .: "short"

instance FromJSON File where
    parseJSON = withObject "file object" $ \ o -> File
        <$> o .: "id"
        <*> o .: "created"
        <*> o .: "timestamp"
        <*> o .: "name"
        <*> o .: "title"
        <*> o .: "mimetype"
        <*> o .: "filetype"
        <*> o .: "pretty_type"
        <*> o .: "user"
        <*> o .: "mode"
        <*> o .: "editable"
        <*> o .: "is_external"
        <*> o .: "external_type"
        <*> o .: "size"
        <*> o .: "url"
        <*> o .: "url_download"
        <*> o .: "url_private"
        <*> o .: "url_private_download"
        <*> parseJSON (Object . HM.fromList . concatMap (\ (k, v) -> maybeToList . map (, v) . stripPrefix "thumb_" $ k) . HM.toList $ o)
        <*> o .: "permalink"
        <*> o .: "edit_link"
        <*> o .: "preview"
        <*> o .: "preview_highlight"
        <*> o .: "lines"
        <*> o .: "lines_more"
        <*> o .: "is_public"
        <*> o .: "public_url_shared"
        <*> o .:? "channels" .!= []
        <*> o .:? "groups" .!= []
        <*> o .:? "ims" .!= []
        <*> o .:? "initial_comment"
        <*> o .:? "num_stars" .!= 0
        <*> o .:? "is_starred" .!= False

instance FromJSON FileMode where
    parseJSON = withText "file mode" $ \ case
        "hosted"   -> pure FileHosted
        "external" -> pure FileExternal
        "snippet"  -> pure FileSnippet
        "post"     -> pure FilePost
        other      -> fail . unpack $ "unknown file mode " <> other

instance FromJSON FileComment where
    parseJSON = withObject "file comment object" $ \ o -> FileComment
        <$> o .: "id"
        <*> o .: "timestamp"
        <*> o .: "user"
        <*> o .: "comment"

instance FromJSON RtmEvent where
    parseJSON v =
        let
            recur :: FromJSON a => Parser a
            recur = parseJSON v
        in flip (withObject "event object") v $ \ o ->
            o .:? "reply_to" >>= \ case
                Just seqnum ->
                    o .: "ok" >>= \ case
                        True  -> RtmReplyOk seqnum <$> o .:? "ts" <*> o .:? "text"
                        False -> o .: "error" >>= (withObject "RTM error" $ \ o2 -> RtmReplyNotOk seqnum <$> o2 .: "code" <*> o2 .: "msg")
                Nothing ->
                    o .: "type" >>= asText >>= \ case
                        "hello"                   -> pure RtmHello
                        "message"                 -> RtmMessage <$> recur
                        "channel_marked"          -> RtmChannelMarked <$> recur
                        "channel_created"         -> RtmChannelCreated <$> o .: "channel"
                        "channel_joined"          -> RtmChannelJoined <$> o .: "channel"
                        "channel_left"            -> RtmChannelLeft <$> o .: "channel"
                        "channel_deleted"         -> RtmChannelDeleted <$> o .: "channel"
                        "channel_rename"          -> RtmChannelRenamed <$> o .: "channel"
                        "channel_archive"         -> RtmChannelArchive <$> recur
                        "channel_unarchive"       -> RtmChannelUnarchive <$> recur
                        "channel_history_changed" -> RtmChannelHistoryChanged <$> recur
                        "im_created"              -> RtmImCreated <$> recur
                        "im_open"                 -> RtmImOpen <$> recur
                        "im_close"                -> RtmImClose <$> recur
                        "im_marked"               -> RtmImMarked <$> recur
                        "im_history_changed"      -> RtmImHistoryChanged <$> recur
                        "group_joined"            -> RtmGroupJoined <$> o .: "channel"
                        "group_left"              -> RtmGroupLeft <$> o .: "channel"
                        "group_open"              -> RtmGroupOpen <$> recur
                        "group_close"             -> RtmGroupClose <$> recur
                        "group_archive"           -> RtmGroupArchive <$> o .: "channel"
                        "group_unarchive"         -> RtmGroupUnarchive <$> o .: "channel"
                        "group_rename"            -> RtmGroupRename <$> o .: "channel"
                        "group_marked"            -> RtmGroupMarked <$> recur
                        "group_history_changed"   -> RtmGroupHistoryChanged <$> recur
                        "file_created"            -> RtmFileCreated <$> o .: "file"
                        "file_shared"             -> RtmFileShared <$> o .: "file"
                        "file_unshared"           -> RtmFileUnshared <$> o .: "file"
                        "file_public"             -> RtmFilePublic <$> o .: "file"
                        "file_private"            -> RtmFilePrivate <$> o .: "file"
                        "file_change"             -> RtmFileChange <$> o .: "file"
                        "file_deleted"            -> RtmFileDeleted <$> recur
                        "file_comment_added"      -> RtmFileCommentAdded <$> recur
                        "file_comment_edited"     -> RtmFileCommentEdited <$> recur
                        "file_comment_deleted"    -> RtmFileCommentDeleted <$> recur
                        "presence_change"         -> RtmPresenceChange <$> recur
                        "manual_presence_change"  -> RtmManualPresenceChange <$> o .: "presence"
                        "user_typing"             -> RtmUserTyping <$> recur
                        "pref_change"             -> RtmPrefChange <$> recur
                        "user_change"             -> RtmUserChange <$> o .: "user"
                        "team_join"               -> RtmTeamJoin <$> o .: "user"
                        "star_added"              -> RtmStarAdded <$> recur
                        "star_removed"            -> RtmStarRemoved <$> recur
                        "emoji_changed"           -> RtmEmojiChanged <$> o .: "event_ts"
                        "commands_changed"        -> RtmCommandsChanged <$> o .: "event_ts"
                        "team_pref_change"        -> RtmTeamPrefChange <$> recur
                        "team_rename"             -> RtmTeamRename <$> o .: "name"
                        "team_domain_change"      -> RtmTeamDomainChange <$> recur
                        "email_domain_changed"    -> RtmEmailDomainChanged <$> recur
                        "bot_added"               -> RtmBotAdded <$> o .: "bot"
                        "bot_changed"             -> RtmBotChanged <$> o .: "bot"
                        "accounts_changed"        -> pure RtmAccountsChanged
                        other                     -> fail . unpack $ "unknown RTM event type " <> other


instance FromJSON (ConversationMarked a) where
    parseJSON = withObject "channel / im / group marked event" $ \ o -> ConversationMarked
        <$> o .: "channel"
        <*> o .: "ts"

instance FromJSON (ConversationUser a) where
    parseJSON = withObject "channel and user from event" $ \ o -> ConversationUser
        <$> o .: "channel"
        <*> o .: "user"

instance FromJSON (ConversationHistoryChanged a) where
    parseJSON = withObject "channel history changed event" $ \ o -> ConversationHistoryChanged
        <$> o .: "latest"
        <*> o .: "ts"
        <*> o .: "event_ts"

instance FromJSON ImCreated where
    parseJSON = withObject "im created event" $ \ o -> ImCreated
        <$> o .: "user"
        <*> o .: "channel"

instance FromJSON FileDeleted where
    parseJSON = withObject "file deleted event" $ \ o -> FileDeleted
        <$> o .: "file_id"
        <*> o .: "event_ts"

instance FromJSON FileCommentUpdated where
    parseJSON = withObject "file comment event" $ \ o -> FileCommentUpdated
        <$> o .: "file"
        <*> o .: "comment"

instance FromJSON FileCommentDeleted where
    parseJSON = withObject "file comment deleted event" $ \ o -> FileCommentDeleted
        <$> o .: "file"
        <*> o .: "comment"

instance FromJSON PresenceChange where
    parseJSON = withObject "presence change event" $ \ o -> PresenceChange
        <$> o .: "user"
        <*> o .: "presence"

instance FromJSON UserTyping where
    parseJSON = withObject "user typing event" $ \ o -> UserTyping
        <$> o .: "user"
        <*> o .: "channel"

instance FromJSON PrefChange where
    parseJSON = withObject "pref change event" $ \ o -> PrefChange
        <$> o .: "name"
        <*> o .: "value"

instance FromJSON Star where
    parseJSON = withObject "star event" $ \ o -> Star
        <$> o .: "user"
        <*> o .: "item"
        <*> o .: "event_ts"

instance FromJSON StarItem where
    parseJSON = withObject "starred item reference" $ \ o -> o .: "type" >>= \ case
        "message"      -> StarItemMessage     <$> o .: "message"
        "file"         -> StarItemFile        <$> o .: "file"
        "file_comment" -> StarItemFileComment <$> o .: "file" <*> o .: "comment"
        "channel"      -> StarItemChannel     <$> o .: "channel"
        "im"           -> StarItemIM          <$> o .: "im"
        "group"        -> StarItemGroup       <$> o .: "group"
        other          -> fail . unpack $ "unknown starrable item type " <> other

instance FromJSON TeamDomainChange where
    parseJSON = withObject "team domain change event" $ \ o -> TeamDomainChange
        <$> o .: "url"
        <*> o .: "domain"

instance FromJSON EmailDomainChanged where
    parseJSON = withObject "email domain changed event" $ \ o -> EmailDomainChanged
        <$> o .: "email_domain"
        <*> o .: "event_ts"

instance ToJSON RtmSendMessage where
    toJSON (RtmSendMessage seqnum conversation message) = object
        [ "type"    .= ("message" :: Text)
        , "id"      .= seqnum
        , "channel" .= conversation
        , "text"    .= message
        ]
