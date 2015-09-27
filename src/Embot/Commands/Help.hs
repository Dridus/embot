module Embot.Commands.Help where

import ClassyPrelude
import Control.Arrow ((>>>), arr)
import Control.Lens (view)
import Data.Attoparsec.Text (Parser, endOfInput, skipSpace)

import Embot.Action (Action(SendMessage))
import Embot.Command (commandChat, simpleCommands)
import Embot.Slack (RtmStartRp)
import Embot.Types (EmbotLogic, MinimalEmbotMonad)

helpFor :: MinimalEmbotMonad m => RtmStartRp -> Parser a -> Text -> [Text] -> EmbotLogic m
helpFor rtmStartRp topic summary details =
  simpleCommands rtmStartRp ("help" *> skipSpace *> topic <* skipSpace <* endOfInput) >>> (arr . map) respond
  where
    respond cmd = singleton . SendMessage (view commandChat cmd) $ summary <> "\n" <> intercalate "\n" details
