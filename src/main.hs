module Main where

import           ClassyPrelude
import           Control.Arrow (returnA)
import           Control.Lens (view)
import           Control.Monad.Logger (logDebug)
import           Control.Wire (Event)
import           Control.Wire.Unsafe.Event (onEventM)
import           TextShow (showt)

import           Embot (EmbotLogic, EmbotWire, LoggingIO, embot, embotDefaultConfig)
import           Embot.Slack (Channel, Group, ID, RtmEvent, RtmStartRp)
import qualified Embot.SlackState as SlackState


main :: IO ()
main = embot config
  where
    config = embotDefaultConfig "xoxb-3212154165-yKrZX5u6Cnfkcy5dYOYfLBhO" app

    app :: RtmStartRp -> (EmbotLogic LoggingIO -> LoggingIO a) -> LoggingIO a
    app rtmStartRp runLogic = do
      let slackState = SlackState.slackState rtmStartRp
      runLogic $ proc event -> do
        channels <- view SlackState.channels slackState -< event
        groups <- view SlackState.groups slackState -< event
        -- ims <- view SlackState.ims slackState -< event
        logEvents -< event
        logChannels -< channels
        logGroups -< groups
        returnA -< mempty

    logChannels :: EmbotWire LoggingIO (Event (Map (ID Channel) Channel)) (Event ())
    logChannels = onEventM $ \ channels -> do
      $logDebug $ "channels are: " ++ showt channels

    logGroups :: EmbotWire LoggingIO (Event (Map (ID Group) Group)) (Event ())
    logGroups = onEventM $ \ groups -> do
      $logDebug $ "groups are: " ++ showt groups

    logEvents :: EmbotWire LoggingIO (Event RtmEvent) (Event ())
    logEvents = onEventM $ \ event -> do
      $logDebug $ "received event: " ++ showt event
