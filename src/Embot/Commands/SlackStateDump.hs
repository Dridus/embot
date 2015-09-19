module Embot.Commands.SlackStateDump (effects) where

import           ClassyPrelude
import           Control.Applicative ((*>), pure)
import           Control.Arrow ((&&&), (>>>), arr)
import           Control.Category ((.))
import           Control.Lens (traverse, to, view)
import           Control.Monad ((>>), guard)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent))
import           Data.Attoparsec.Text (skipSpace)
import           Data.Foldable (foldMap)
import           Data.Function (($))
import           Data.Maybe (Maybe(Just), catMaybes)
import           Data.Monoid ((<>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Text.Show.Text (show)

import           Embot.Action (Action(SendMessage))
import           Embot.Command (commandChat, simpleCommands)
import           Embot.Slack (RtmStartRp, channelName, channelIsArchived, channelIsGeneral, channelMembers)
import           Embot.SlackState (SlackState, channels)
import           Embot.Types (EmbotLogic, MinimalEmbotMonad)

effects :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
effects rtmStartRp slackState = foldMap (($ rtmStartRp) >>> ($ slackState))
    [ dumpChannels
    , test
    ]

test :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
test rtmStartRp slackState = simpleCommands rtmStartRp ("test") >>> arr (map $ \ cmd -> Seq.singleton (SendMessage (view commandChat cmd) ("bleh! " <> show cmd)))

dumpChannels :: MinimalEmbotMonad m => RtmStartRp -> SlackState m -> EmbotLogic m
-- FIXME I'm 100% positive I don't understand netwire enough and this could be implemented better. maybe something like fix $ (-->) …?
dumpChannels rtmStartRp slackState = (view channels slackState &&& simpleCommands rtmStartRp ("show" *> skipSpace *> "channels")) >>> arr f
  where
    f (chans, Event cmd) = Event . Seq.singleton $ SendMessage (view commandChat cmd) (T.intercalate "\n" . map formatChan $ chans)
    f _ = NoEvent

    formatChan = do
        name <- view channelName
        archived <- view channelIsArchived
        general <- view channelIsGeneral
        members <- view channelMembers
        let tags = T.intercalate " " . catMaybes
                     $ [ guard archived >> pure "(archived)"
                       , guard general >> pure "(general)"
                       , Just $ "(" <> show (length members) <> " members)"
                       ]
        pure $ "#" <> name <> " " <> tags

