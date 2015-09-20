module Embot.Slack where

import           ClassyPrelude
import           Control.Lens (Getter, view, to)
import           Control.Lens.TH (makeLenses, makePrisms)
import           Data.Aeson ((.:), (.:?), (.=), (.!=), Value(Object, String), Object, FromJSON(parseJSON), ToJSON(toJSON), object, withText, withObject, withScientific, withText)
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (toBoundedInteger)
import           TextShow (FromStringShow(FromStringShow), TextShow(showb), showt)
import           TextShow.TH (deriveTextShow)

import           Embot.TextShowOrphans ()

newtype TS = TS { unTS :: Text } deriving (Eq, Ord)
instance FromJSON TS where
  parseJSON = withText "timestamp" $ pure . TS
deriveTextShow ''TS

newtype Time = Time { unTime :: Word32 } deriving (Eq, Ord)
instance FromJSON Time where
  parseJSON = withScientific "time" $ \ s ->
    case toBoundedInteger s of
      Just w32 -> pure (Time w32)
      Nothing  -> fail . unpack $ "out of bound unix time " <> showt (FromStringShow s)
deriveTextShow ''Time

newtype ID a = ID { unID :: Text } deriving (Eq, Ord)
instance FromJSON (ID a) where
  parseJSON = withText "id" $ pure . ID
instance ToJSON (ID a) where
  toJSON = String . unID
deriveTextShow ''ID

idedName :: Getter s Text -> Getter s (ID k) -> (s -> Text)
idedName name ident s = view name s ++ " <" ++ view (ident . to unID) s ++ ">"

data Response a = ResponseNotOk !Text | ResponseOk a

data RtmStartRequest = RtmStartRequest { rtmStartToken :: Text }

data RtmStartRp = RtmStartRp
  { _rtmStartUrl      :: Text
  , _rtmStartSelf     :: Self
  , _rtmStartTeam     :: Team
  , _rtmStartUsers    :: [User]
  , _rtmStartChannels :: [Channel]
  , _rtmStartGroups   :: [Group]
  , _rtmStartIMs      :: [IM]
  , _rtmStartBots     :: [Bot] }

data Self = Self
  { _selfID             :: ID User
  , _selfName           :: Text
  , _selfPrefs          :: Object
  , _selfCreated        :: Time
  , _selfManualPresence :: Presence }

data Presence = PresenceActive | PresenceAway

data Team = Team
  { _teamID                :: ID Team
  , _teamName              :: Text
  , _teamEmailDomain       :: Maybe Text
  , _teamDomain            :: Text
  , _teamMsgEditWindowMins :: Maybe Int
  , _teamOverStorageLimit  :: Bool
  , _teamPrefs             :: Object }

data User = User
  { _userID                :: ID User
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
  , _profilePhone     :: Maybe Text }

data Channel = Channel
  { _channelID          :: ID Channel
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
  , _channelUnreadCount :: Maybe Int }

data Group = Group
  { _groupID          :: ID Group
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
  , _groupUnreadCount :: Maybe Int }

data IM = IM
  { _imID            :: ID IM
  , _imUser          :: ID User
  , _imCreated       :: Time
  , _imIsUserDeleted :: Bool
  , _imIsOpen        :: Bool
  , _imLastRead      :: Maybe TS
  , _imLatest        :: Maybe Message
  , _imUnreadCount   :: Maybe Int }

data Bot = Bot
  { _botID    :: ID Bot
  , _botName  :: Text
  , _botIcons :: HM.HashMap Text Text }

data Chat

data Message = Message
  { _messageChat         :: Maybe (ID Chat)
  , _messageUser         :: ID User
  , _messageSubtype      :: Maybe MessageSubtype
  , _messageText         :: Text
  , _messageTS           :: TS
  , _messageEdited       :: Maybe MessageEdited
  , _messageDeletedTS    :: Maybe TS
  , _messageEventTS      :: Maybe TS
  , _messageHidden       :: Bool
  , _messageAttachments  :: [Attachment]
  , _messageInviter      :: Maybe (ID User) }

data MessageSubtype
  = BotMS | MeMS | ChangedMS | DeletedMS
  | ChannelJoinMS | ChannelLeaveMS | ChannelTopicMS | ChannelPurposeMS | ChannelNameMS | ChannelArchiveMS | ChannelUnarchiveMS
  | GroupJoinMS   | GroupLeaveMS   | GroupTopicMS   | GroupPurposeMS   | GroupNameMS   | GroupArchiveMS   | GroupUnarchiveMS
  | FileShareMS | FileCommentMS | FileMentionMS

data MessageEdited = MessageEdited
  { _messageEditedUser :: ID User
  , _messageEditedTS   :: TS }

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
  , _attachmentFields     :: [AttachmentField] }

data AttachmentField = AttachmentField
  { _fieldTitle :: Text
  , _fieldValue :: Text
  , _fieldShort :: Bool }

data SlackTracked a = SlackTracked
  { _trackedValue   :: a
  , _trackedCreator :: ID User
  , _trackedLastSet :: Time }

data File = File
  { _fileID                 :: ID File
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
  , _fileIsStarred          :: Bool }

data FileMode
  = FileHosted
  | FileExternal
  | FileSnippet
  | FilePost

data FileComment = FileComment
  { _fileCommentID        :: ID FileComment
  , _fileCommentTimestamp :: Time
  , _fileCommentUser      :: ID User
  , _fileCommentComment   :: Text }

data RtmEvent
  = RtmHello
  | RtmReplyOk Word64 (Maybe TS) (Maybe Text)
  | RtmReplyNotOk Word64 Int32 Text
  | RtmMessage Message
  | RtmChannelMarked (ChatMarked Channel)
  | RtmChannelCreated Channel
  | RtmChannelJoined Channel
  | RtmChannelLeft (ID Channel)
  | RtmChannelDeleted (ID Channel)
  | RtmChannelRenamed (ChatRenamed Channel)
  | RtmChannelArchive (ChatUser Channel)
  | RtmChannelUnarchive (ChatUser Channel)
  | RtmChannelHistoryChanged (ChatHistoryChanged Channel)
  | RtmImCreated ImCreated
  | RtmImOpen (ChatUser IM)
  | RtmImClose (ChatUser IM)
  | RtmImMarked (ChatMarked IM)
  | RtmImHistoryChanged (ChatHistoryChanged IM)
  | RtmGroupJoined Group
  | RtmGroupLeft (ID Group)
  | RtmGroupOpen (ChatUser Group)
  | RtmGroupClose (ChatUser Group)
  | RtmGroupArchive (ID Group)
  | RtmGroupUnarchive (ID Group)
  | RtmGroupRename (ChatRenamed Group)
  | RtmGroupMarked (ChatMarked Group)
  | RtmGroupHistoryChanged (ChatHistoryChanged Group)
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

data ChatMarked a = ChatMarked
  { _chatMarkedChannel :: ID a
  , _chatMarkedTS      :: TS }

data ChatUser a = ChatUser
  { _chatUserUser      :: ID User
  , _chatUserChannelID :: ID a }

data ChatRenamed a = ChatRenamed
  { _chatRenamedChannelID :: ID a
  , _chatRenamedName      :: Text }

data ChatHistoryChanged a = ChatHistoryChanged
  { _chatHistoryChangedLatest  :: Text
  , _chatHistoryChangedTS      :: TS
  , _chatHistoryChangedEventTS :: TS }

data ImCreated = ImCreated
  { _imCreatedUser    :: Text
  , _imCreatedChannel :: Channel }

data FileDeleted = FileDeleted
  { _fileDeletedFileID  :: Text
  , _fileDeletedEventTS :: Text }

data FileCommentUpdated = FileCommentUpdated
  { _fileCommentUpdatedFile    :: File
  , _fileCommentUpdatedComment :: FileComment }

data FileCommentDeleted = FileCommentDeleted
  { _fileCommentDeletedFile    :: File
  , _fileCommentDeletedComment :: ID FileComment }

data PresenceChange = PresenceChange
  { _presenceChangeUser     :: ID User
  , _presenceChangePresence :: Presence }

data PrefChange = PrefChange
  { _prefChangeName  :: Text
  , _prefChangeValue :: Value }

data UserTyping = UserTyping
  { _userTypingUser    :: ID User
  , _userTypingChannel :: ID Chat }

data Star = Star
  { _starUser    :: Text
  , _starItem    :: StarItem
  , _starEventTS :: TS }

data StarItem
  = StarItemMessage Message
  | StarItemFile File
  | StarItemFileComment File FileComment
  | StarItemChannel (ID Channel)
  | StarItemIM (ID IM)
  | StarItemGroup (ID Group)

data TeamDomainChange = TeamDomainChange
  { _teamDomainChangeUrl    :: Text
  , _teamDomainChangeDomain :: Text }

data EmailDomainChanged = EmailDomainChanged
  { _emailDomainChangedEmailDomain :: Text
  , _emailDomainChangedEventTS     :: TS }

data RtmSendMessage = RtmSendMessage
  { _sendMessageSeqnum :: Word64
  , _sendMessageChat   :: ID Chat
  , _sendMessageText   :: Text }

class SlackTyped a where
  isTypedID :: Proxy a -> ID b -> Bool
instance SlackTyped Channel where
  isTypedID _ = isPrefixOf "C" . unID
instance SlackTyped File where
  isTypedID _ (ID t) = "F" `isPrefixOf` t && not ("Fc" `isPrefixOf` t)
instance SlackTyped FileComment where
  isTypedID _ (ID t) = "Fc" `isPrefixOf` t
instance SlackTyped Group where
  isTypedID _ = isPrefixOf "G" . unID
instance SlackTyped Chat where
   isTypedID _ i
    =  isTypedID (Proxy :: Proxy Channel) i
    || isTypedID (Proxy :: Proxy IM) i
    || isTypedID (Proxy :: Proxy Group) i
instance SlackTyped IM where
  isTypedID _ = isPrefixOf "D" . unID
instance SlackTyped User where
  isTypedID _ = isPrefixOf "U" . unID

asTypedID :: forall a b. SlackTyped b => ID a -> Maybe (ID b)
asTypedID i =
  if isTypedID (Proxy :: Proxy b) i
    then Just (ID . unID $ i)
    else Nothing

asChannelID :: ID Chat -> Maybe (ID Channel)
asChannelID = asTypedID
asGroupID :: ID Chat -> Maybe (ID Group)
asGroupID = asTypedID
asIMID :: ID Chat -> Maybe (ID IM)
asIMID = asTypedID

deriving instance Eq RtmStartRequest
deriving instance Eq RtmStartRp
deriving instance Eq Self
deriving instance Eq Team
deriving instance Eq User
deriving instance Eq Profile
deriving instance Eq Channel
deriving instance Eq Group
deriving instance Eq IM
deriving instance Eq Bot
deriving instance Eq MessageSubtype
deriving instance Eq Message
deriving instance Eq MessageEdited
deriving instance Eq Attachment
deriving instance Eq AttachmentField
deriving instance Eq a => Eq (SlackTracked a)
deriving instance Eq FileMode
deriving instance Eq File
deriving instance Eq FileComment
deriving instance Eq RtmEvent
deriving instance Eq a => Eq (ChatMarked a)
deriving instance Eq a => Eq (ChatUser a)
deriving instance Eq a => Eq (ChatRenamed a)
deriving instance Eq a => Eq (ChatHistoryChanged a)
deriving instance Eq ImCreated
deriving instance Eq FileDeleted
deriving instance Eq FileCommentUpdated
deriving instance Eq FileCommentDeleted
deriving instance Eq Presence
deriving instance Eq PresenceChange
deriving instance Eq UserTyping
deriving instance Eq PrefChange
deriving instance Eq Star
deriving instance Eq StarItem
deriving instance Eq TeamDomainChange
deriving instance Eq EmailDomainChanged
deriving instance Eq RtmSendMessage

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
makeLenses ''ChatMarked
makeLenses ''ChatUser
makeLenses ''ChatRenamed
makeLenses ''ChatHistoryChanged
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


instance TextShow Chat where
  showb _ = "Chat"

deriveTextShow ''RtmStartRequest
deriveTextShow ''RtmStartRp
deriveTextShow ''Self
deriveTextShow ''Presence
deriveTextShow ''Team
deriveTextShow ''User
deriveTextShow ''Profile
deriveTextShow ''Channel
deriveTextShow ''Group
deriveTextShow ''IM
deriveTextShow ''Bot
deriveTextShow ''Message
deriveTextShow ''MessageSubtype
deriveTextShow ''MessageEdited
deriveTextShow ''Attachment
deriveTextShow ''AttachmentField
deriveTextShow ''SlackTracked
deriveTextShow ''File
deriveTextShow ''FileMode
deriveTextShow ''FileComment
deriveTextShow ''RtmEvent
deriveTextShow ''ChatMarked
deriveTextShow ''ChatUser
deriveTextShow ''ChatRenamed
deriveTextShow ''ChatHistoryChanged
deriveTextShow ''ImCreated
deriveTextShow ''FileDeleted
deriveTextShow ''FileCommentUpdated
deriveTextShow ''FileCommentDeleted
deriveTextShow ''PresenceChange
deriveTextShow ''UserTyping
deriveTextShow ''PrefChange
deriveTextShow ''Star
deriveTextShow ''StarItem
deriveTextShow ''TeamDomainChange
deriveTextShow ''EmailDomainChanged
deriveTextShow ''RtmSendMessage

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
    let recur :: FromJSON a => Parser a
        recur = parseJSON v
    in flip (withObject "event object") v $ \ o ->
        o .:? "reply_to" >>= \ case
          Just seqnum ->
            o .: "ok" >>= \ case
              True  -> RtmReplyOk seqnum <$> o .:? "ts" <*> o .:? "text"
              False -> o .: "error" >>= (withObject "RTM error" $ \ o2 -> RtmReplyNotOk seqnum <$> o2 .: "code" <*> o2 .: "msg")
          Nothing ->
            o .: "type" >>= pure . asText >>= \ case
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


instance FromJSON (ChatMarked a) where
  parseJSON = withObject "channel / im / group marked event" $ \ o -> ChatMarked
    <$> o .: "channel"
    <*> o .: "ts"

instance FromJSON (ChatUser a) where
  parseJSON = withObject "channel and user from event" $ \ o -> ChatUser
    <$> o .: "channel"
    <*> o .: "user"

instance FromJSON (ChatRenamed a) where
  parseJSON = withObject "channel and new name from event" $ \ o -> ChatRenamed
    <$> o .: "id"
    <*> o .: "name"

instance FromJSON (ChatHistoryChanged a) where
  parseJSON = withObject "channel history changed event" $ \ o -> ChatHistoryChanged
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
  parseJSON = withObject "starred item reference" $ \ o -> o .: "type" >>= pure . asText >>= \ case
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
  toJSON (RtmSendMessage seqnum chat message) = object
    [ "type"    .= ("message" :: Text)
    , "id"      .= seqnum
    , "channel" .= chat
    , "text"    .= message
    ]
