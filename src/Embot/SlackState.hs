module Embot.SlackState
  ( SlackState(..), slackState, channels, ims, groups, users
  ) where

import           ClassyPrelude
import           Control.Arrow ((>>>))
import           Control.Lens ((.~), to, view)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Logger (logInfo)
import           Control.Wire (Event)
import           Data.Foldable (for_, traverse_)
import qualified Data.Map.Strict as Map
import           TextShow (showt)

import Embot.Slack ( chatRenamedChannelID, chatRenamedName
                   , Channel, channelID, channelIsArchived, channelIsMember, channelName
                   , chatUserChannelID, imCreatedChannel, presenceChangePresence, presenceChangeUser
                   , Group, groupID, groupIsArchived, groupName
                   , IM, imID, imUser, imIsOpen
                   , ID, idedName
                   , User, userID, userName, userPresence
                   , RtmEvent( RtmChannelCreated, RtmChannelJoined, RtmChannelLeft, RtmChannelDeleted
                             , RtmChannelRenamed, RtmChannelArchive, RtmChannelUnarchive
                             , RtmGroupJoined, RtmGroupLeft, RtmGroupRename, RtmGroupArchive, RtmGroupUnarchive
                             , RtmIMCreated, RtmIMOpen, RtmIMClose
                             , RtmUserChange, RtmTeamJoin, RtmPresenceChange )
                   , RtmStartRp, rtmStartChannels, rtmStartGroups, rtmStartIMs, rtmStartUsers )
import Embot.Types (EmbotWire, MinimalEmbotMonad)
import Embot.Wire (accumEM, filterSameE)

data SlackState m = SlackState
    { _channels :: EmbotWire m (Event RtmEvent) (Event (Map (ID Channel) Channel))
    , _groups   :: EmbotWire m (Event RtmEvent) (Event (Map (ID Group)   Group))
    , _ims      :: EmbotWire m (Event RtmEvent) (Event (Map (ID IM)      IM))
    , _users    :: EmbotWire m (Event RtmEvent) (Event (Map (ID User)    User)) }

makeLenses ''SlackState

slackState :: MinimalEmbotMonad m => RtmStartRp -> SlackState m
slackState = SlackState <$> channelsWire <*> groupsWire <*> imsWire <*> usersWire

channelsWire :: forall m. MinimalEmbotMonad m => RtmStartRp -> EmbotWire m (Event RtmEvent) (Event (Map (ID Channel) Channel))
channelsWire rtmStartRp =
  accumEM k (Map.fromList . map (view channelID &&& id) $ view rtmStartChannels rtmStartRp) >>> filterSameE
  where
    k chans = (($ chans) <$>) . \ case
      RtmChannelCreated chan -> do
        logCh "Channel created" chan
        pure $ Map.insert (view channelID chan) chan
      RtmChannelJoined chan -> do
        logCh "Channel joined" chan
        pure $ Map.insert (view channelID chan) chan
      RtmChannelLeft chanID -> do
        logId "Channel left" chanID chans
        pure $ Map.adjust (channelIsMember .~ False) chanID
      RtmChannelRenamed chanRename -> do
        let chanID = view chatRenamedChannelID chanRename
        logId "Channel renamed" chanID chans
        pure $ Map.adjust (channelName .~ view chatRenamedName chanRename) chanID
      RtmChannelDeleted chanID -> do
        logId "Channel deleted" chanID chans
        pure $ Map.delete chanID
      RtmChannelArchive (view chatUserChannelID -> chanID) -> do
        logId "Channel archived" chanID chans
        pure $ Map.adjust (channelIsArchived .~ True) chanID
      RtmChannelUnarchive (view chatUserChannelID -> chanID) -> do
        logId "Channel unarchived" chanID chans
        pure $ Map.adjust (channelIsArchived .~ False) chanID
      _ ->
        pure id

    logCh :: Text -> Channel -> m ()
    logCh prefix channel =
      $logInfo $ prefix ++ " "
              ++ (showt . view channelName $ channel)
              ++ " (" ++ (showt . view channelID $ channel) ++ ")"

    logId :: Text -> ID Channel -> Map (ID Channel) Channel -> m ()
    logId prefix chanID =
      traverse_ (logCh prefix) . headMay . Map.lookup chanID

groupsWire :: forall m. MinimalEmbotMonad m => RtmStartRp -> EmbotWire m (Event RtmEvent) (Event (Map (ID Group) Group))
groupsWire rtmStartRp =
  accumEM k (Map.fromList . map (view groupID &&& id) $ view rtmStartGroups rtmStartRp) >>> filterSameE
  where
    k grps = (($ grps) <$>) . \ case
      RtmGroupJoined grp -> do
        logGr "Group joined" grp
        pure $ Map.insert (view groupID grp) grp
      RtmGroupLeft grpID -> do
        logId "Group left" grpID grps
        pure $ Map.delete grpID
      RtmGroupRename groupRename -> do
        let grpID = view chatRenamedChannelID groupRename
        logId "Group renamed" grpID grps
        pure $ Map.adjust (groupName .~ view chatRenamedName groupRename) grpID
      RtmGroupArchive grpID -> do
        logId "Group archived" grpID grps
        pure $ Map.adjust (groupIsArchived .~ True) grpID
      RtmGroupUnarchive grpID -> do
        logId "Group unarchived" grpID grps
        pure $ Map.adjust (groupIsArchived .~ False) grpID
      _ ->
        pure id

    logGr :: Text -> Group -> m ()
    logGr prefix grp =
      $logInfo $ prefix ++ " " ++ idedName groupName groupID grp

    logId :: Text -> ID Group -> Map (ID Group) Group -> m ()
    logId prefix grpID =
      traverse_ (logGr prefix) . headMay . Map.lookup grpID

imsWire :: forall m. MinimalEmbotMonad m => RtmStartRp -> EmbotWire m (Event RtmEvent) (Event (Map (ID IM) IM))
imsWire rtmStartRp =
  accumEM k (Map.fromList . map (view imID &&& id) $ view rtmStartIMs rtmStartRp) >>> filterSameE
  where
    k is = (($ is) <$>) . \ case
      RtmIMCreated (view imCreatedChannel -> i) -> do
        logIm "IM created" i
        pure $ Map.insert (view imID i) i
      RtmIMOpen (view chatUserChannelID -> iID) -> do
        logId "IM opened" iID is
        pure $ Map.adjust (imIsOpen .~ True) iID
      RtmIMClose (view chatUserChannelID -> iID) -> do
        logId "IM closed" iID is
        pure $ Map.adjust (imIsOpen .~ False) iID
      _ ->
        pure id

    logIm :: Text -> IM -> m ()
    logIm prefix i = $logInfo $ prefix ++ " " ++ idedName (to $ const "IM") imUser i

    logId :: Text -> ID IM -> Map (ID IM) IM -> m ()
    logId prefix iID =
      traverse_ (logIm prefix) . headMay . Map.lookup iID

usersWire :: MinimalEmbotMonad m => RtmStartRp -> EmbotWire m (Event RtmEvent) (Event (Map (ID User) User))
usersWire rtmStartRp =
  accumEM k (Map.fromList . map (view userID &&& id) $ view rtmStartUsers rtmStartRp) >>> filterSameE
  where
    k us = (($ us) <$>) . \ case
      RtmUserChange user -> do
        $logInfo $ "User changed " ++ idedName userName userID user
        pure $ Map.insert (view userID user) user
      RtmTeamJoin user -> do
        $logInfo $ "User joined " ++ idedName userName userID user
        pure $ Map.insert (view userID user) user
      RtmPresenceChange change -> do
        let uID = view presenceChangeUser change
            presence = view presenceChangePresence change
        for_ (Map.lookup uID us) $ \ user ->
          $logInfo $ "User " ++ idedName userName userID user ++ " changed presence to " ++ showt presence
        pure $ Map.adjust (userPresence .~ Just presence) uID
      _ ->
        pure id


