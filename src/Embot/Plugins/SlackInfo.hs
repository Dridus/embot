module Embot.Plugins.SlackInfo where

import           ClassyPrelude (map)
import           Control.Applicative (pure)
import           Control.Arrow ((&&&))
import           Control.Category ((.))
import           Control.Lens ((^.), view)
import           Control.Lens.TH (makeLenses)
import           Data.Function (($), id)
import           Data.HList (HList(HCons), onTIP)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Embot.Core (EnvInitializer, NotEnvElem)
import Embot.SlackAPI
    ( ID
    , Channel, channelId, Group, groupId, IM, imId, Bot, botId, Self, Team, User, userId
    , RtmStartRp, rtmStartSelf, rtmStartTeam, rtmStartChannels, rtmStartUsers, rtmStartGroups, rtmStartIms, rtmStartBots
    )

data SlackInfo = SlackInfo
    { _infoSelf     :: Self
    , _infoTeam     :: Team
    , _infoUsers    :: Map (ID User) User
    , _infoChannels :: Map (ID Channel) Channel
    , _infoGroups   :: Map (ID Group) Group
    , _infoIms      :: Map (ID IM) IM
    , _infoBots     :: Map (ID Bot) Bot
    }

makeLenses ''SlackInfo

fromStartRp :: RtmStartRp -> SlackInfo
fromStartRp rp = SlackInfo
    { _infoSelf     = rp ^. rtmStartSelf
    , _infoTeam     = rp ^. rtmStartTeam
    , _infoUsers    = Map.fromList . map (view userId    &&& id) $ rp ^. rtmStartUsers
    , _infoChannels = Map.fromList . map (view channelId &&& id) $ rp ^. rtmStartChannels
    , _infoGroups   = Map.fromList . map (view groupId   &&& id) $ rp ^. rtmStartGroups
    , _infoIms      = Map.fromList . map (view imId      &&& id) $ rp ^. rtmStartIms
    , _infoBots     = Map.fromList . map (view botId     &&& id) $ rp ^. rtmStartBots
    }

slackInfoEnv :: NotEnvElem SlackInfo es => RtmStartRp -> EnvInitializer es (SlackInfo ': es)
slackInfoEnv startRp = pure . onTIP (HCons $ fromStartRp startRp)
