module Main where

import           ClassyPrelude
import           Control.Monad.Logger (logDebug)
import           Control.Wire.Unsafe.Event (onEventM)
import           System.Environment (getEnv)
import           TextShow (showt)

import           Embot (EmbotLogic, LoggingIO, embot, embotDefaultConfig)
import           Embot.Command (Mention, mentions)
import qualified Embot.Commands.SlackStateDump as SlackStateDump
import           Embot.Slack (RtmEvent, RtmStartRp)
import qualified Embot.SlackState as SlackState
import           Embot.SlackStateChangelog (slackStateLogger)

main :: IO ()
main = do
  apiKey <- pack <$> getEnv "EMBOT_API_KEY"

  let
    config = embotDefaultConfig apiKey app

    app :: RtmStartRp -> (EmbotLogic LoggingIO -> LoggingIO a) -> LoggingIO a
    app rtmStartRp runLogic = do
      let slackState = SlackState.slackState rtmStartRp
      runLogic $ proc event -> do
        mention <- mentions rtmStartRp -< event
        slackStateLogger slackState -< event
        onEventM logEvent -< event
        onEventM logMention -< mention

        SlackStateDump.effects rtmStartRp slackState -< event

    logEvent :: RtmEvent -> LoggingIO ()
    logEvent event =
      $logDebug $ "received event: " ++ showt event

    logMention :: Mention -> LoggingIO ()
    logMention mention =
      $logDebug $ "received mention: " ++ showt mention

  embot config
