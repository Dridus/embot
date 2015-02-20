module Embot.Plugins.SlackInfoCommands where

import           ClassyPrelude (map)
import           Control.Applicative (pure)
import           Control.Category ((.))
import           Control.Lens (Getter, (&), (.~), to, use)
import           Data.Bool (Bool(True))
import           Data.Function (($))
import qualified Data.Map.Strict as Map
import           Text.Show.Text (Show, show)

import Embot.Core (EnvElem, EnvInitializer, Interceptor, InterceptorInitializer, env)
import Embot.Event (eventConsumed)
import Embot.Helpers (replyTo)
import Embot.Plugins.Commands (Command, Commands, addCommand, devCommand, onCommand)
import Embot.Plugins.SlackInfo (SlackInfo, infoSelf, infoTeam, infoUsers, infoChannels, infoGroups, infoIMs, infoBots)

showSelfCommand :: Command
showSelfCommand = devCommand "show self" "Show the self information from the RTM start"

showTeamCommand :: Command
showTeamCommand = devCommand "show team" "Show the team information"

showUsersCommand :: Command
showUsersCommand = devCommand "show users" "Show a list of known users"

showChannelsCommand :: Command
showChannelsCommand = devCommand "show channels" "Show a list of known channels"

showGroupsCommand :: Command
showGroupsCommand = devCommand "show groups" "Show a list of known groups"

showIMsCommand :: Command
showIMsCommand = devCommand "show ims" "Show a list of known IMs"

showBotsCommand :: Command
showBotsCommand = devCommand "show bots" "Show a list of known bots"


slackInfoCommandsEnv :: EnvElem Commands es => EnvInitializer es es
slackInfoCommandsEnv =
      pure
    . addCommand showSelfCommand
    . addCommand showTeamCommand
    . addCommand showUsersCommand
    . addCommand showChannelsCommand
    . addCommand showGroupsCommand
    . addCommand showIMsCommand
    . addCommand showBotsCommand

slackInfoCommands :: forall (es :: [*]) (is :: [*]). (EnvElem Commands es, EnvElem SlackInfo es) => InterceptorInitializer es is is
slackInfoCommands (next, nextState) = pure $ (commands next, nextState)
  where
    commands =
          onCommand showSelfCommand handleShowSelf
        . onCommand showTeamCommand handleShowTeam
        . onCommand showUsersCommand handleShowUsers
        . onCommand showChannelsCommand handleShowChannels
        . onCommand showGroupsCommand handleShowGroups
        . onCommand showIMsCommand handleShowIMs
        . onCommand showBotsCommand handleShowBots

    useAndShow :: Show a => Getter SlackInfo [a] -> Interceptor es is
    useAndShow l event = do
        as <- use $ env . l
        replyTo event $ map show as
        next $ event & eventConsumed .~ True

    handleShowSelf     = useAndShow $ infoSelf     . to (:[])
    handleShowTeam     = useAndShow $ infoTeam     . to (:[])
    handleShowUsers    = useAndShow $ infoUsers    . to Map.elems
    handleShowChannels = useAndShow $ infoChannels . to Map.elems
    handleShowGroups   = useAndShow $ infoGroups   . to Map.elems
    handleShowIMs      = useAndShow $ infoIMs      . to Map.elems
    handleShowBots     = useAndShow $ infoBots     . to Map.elems
