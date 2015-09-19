module Embot.Examples.Echo
  ( echoLogic
  ) where

import           ClassyPrelude
import           Control.Arrow (arr)
import           Control.Lens (view)
import           Data.Function (($))
import           Data.Maybe (Maybe(Just))
import qualified Data.Sequence as Seq
import           Embot.Action (Action(SendMessage))
import qualified Embot.Slack as Slack
import           Embot.Types (EmbotLogic, LoggingIO)
import           TextShow (showt)

echoLogic :: EmbotLogic LoggingIO
echoLogic = arr $ map respond
  where
    respond (Slack.RtmMessage message@(view Slack.messageChat -> Just chat)) =
      Seq.singleton (SendMessage chat $ showt message)
    respond _ =
      Seq.empty
