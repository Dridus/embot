module Embot.Commands.SlackStateDump where

import           ClassyPrelude
import           Control.Arrow ((<<<), (>>>), returnA)
import           Control.Lens (_1, _2, to, view)
import           Control.Wire (hold)
import           Data.Attoparsec.Text (endOfInput, skipSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           TextShow (showt)

import           Embot.Action (Action(SendMessage))
import           Embot.Command (Command, commandChat, simpleCommands)
import           Embot.Slack ( RtmStartRp
                             , Channel, channelName, channelIsArchived, channelIsGeneral, channelMembers
                             , Group, groupName, groupIsArchived, groupMembers
                             , IM, imUser, imIsOpen
                             , User, userName, userRealName, userPresence, userIsAdmin, userIsOwner, userIsPrimaryOwner, userIsRestricted, userIsUltraRestricted
                             , ID, unID
                             , Presence(PresenceAway) )
import           Embot.SlackState (SlackState)
import qualified Embot.SlackState as SlackState
import           Embot.Types (EmbotLogic, MinimalEmbotMonad)

effects :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
effects rtmStartRp slackState = foldMap (($ rtmStartRp) >>> ($ slackState))
    [ showChannels
    , showGroups
    , showIMs
    , showUsers
    ]

showChannels :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
showChannels rtmStartRp slackState =
  proc event -> do
    channels <- hold <<< view SlackState.channels slackState -< event
    command <- simpleCommands rtmStartRp $ "show" *> skipSpace *> "channels" *> endOfInput -< event
    returnA -< map (respond channels) command
  where
    respond :: Map (ID Channel) Channel -> Command a -> Seq Action
    respond channels cmd = Seq.singleton $ SendMessage (view commandChat cmd) (intercalate "\n" . map formatChan . sortBy (compare `on` view channelName) . toList $ channels)

    formatChan :: Channel -> Text
    formatChan = do
      name <- view channelName
      archived <- view channelIsArchived
      general <- view channelIsGeneral
      members <- view channelMembers
      let tags = intercalate " " . catMaybes
                    $ [ guard archived >> pure "(archived)"
                      , guard general >> pure "(general)"
                      , Just $ "(" <> showt (length members) <> " members)" ]
      pure $ "#" <> name <> " " <> tags

showGroups :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
showGroups rtmStartRp slackState =
  proc event -> do
    groups <- hold <<< view SlackState.groups slackState -< event
    command <- simpleCommands rtmStartRp $ "show" *> skipSpace *> "groups" *> endOfInput -< event
    returnA -< map (respond groups) command
  where
    respond :: Map (ID Group) Group -> Command a -> Seq Action
    respond groups cmd = Seq.singleton $ SendMessage (view commandChat cmd) (intercalate "\n" . map formatGroup . sortBy (compare `on` view groupName) . toList $ groups)

    formatGroup :: Group -> Text
    formatGroup = do
      name <- view groupName
      archived <- view groupIsArchived
      members <- view groupMembers
      let tags = intercalate " " . catMaybes
                    $ [ guard archived >> pure "(archived)"
                      , Just $ "(" <> showt (length members) <> " members)" ]
      pure $ name <> " " <> tags

showIMs :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
showIMs rtmStartRp slackState =
  proc event -> do
    ims <- hold <<< view SlackState.ims slackState -< event
    users <- hold <<< view SlackState.users slackState -< event
    command <- simpleCommands rtmStartRp $ "show" *> skipSpace *> "ims" *> endOfInput -< event
    returnA -< map (respond users ims) command
  where
    respond :: Map (ID User) User -> Map (ID IM) IM -> Command a -> Seq Action
    respond users ims (view commandChat -> chat) =
      let imLines = map formatIM
                  . sortBy (compare `on` fst)
                  . map addUserName
                  . toList $ ims
          addUserName im =
            let name = fromMaybe (view (imUser . to unID) im) . map (view userName) $ Map.lookup (view imUser im) users
            in (name, im)
      in Seq.singleton $ SendMessage chat (intercalate "\n" imLines)

    formatIM :: (Text, IM) -> Text
    formatIM = do
      name <- view _1
      open <- view (_2 . imIsOpen)
      let tags = intercalate " " . catMaybes
                    $ [ guard open >> pure "(open)" ]
      pure $ name <> " " <> tags

showUsers :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
showUsers rtmStartRp slackState =
  proc event -> do
    users <- hold <<< view SlackState.users slackState -< event
    command <- simpleCommands rtmStartRp $ "show" *> skipSpace *> "users" *> endOfInput -< event
    returnA -< map (respond users) command
  where
    respond :: Map (ID User) User -> Command a -> Seq Action
    respond users (view commandChat -> chat) =
      let userLines = map formatUser
                    . sortBy (compare `on` view userName)
                    . toList $ users
      in Seq.singleton $ SendMessage chat (intercalate "\n" userLines)

    formatUser :: User -> Text
    formatUser = do
      name            <- view userName
      realName        <- view userRealName
      admin           <- view userIsAdmin
      owner           <- view userIsOwner
      primaryOwner    <- view userIsPrimaryOwner
      restricted      <- view userIsRestricted
      ultraRestricted <- view userIsUltraRestricted
      presenceAway    <- view (userPresence . to (== Just PresenceAway))
      let tags = intercalate " " . catMaybes
                    $ [ guard admin >> pure "(admin)"
                      , guard owner >> pure "(owner)"
                      , guard primaryOwner >> pure "(primary owner)"
                      , guard restricted >> pure "(restricted)"
                      , guard ultraRestricted >> pure "(ultra restricted)"
                      , guard presenceAway >> pure "(away)" ]
      pure $ "@" <> name <> " (" <> realName <> ") " <> tags
