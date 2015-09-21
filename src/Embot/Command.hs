module Embot.Command
    ( Mention(..), mentionText, mentionChat, mentionMessage, mentions
    , Command(..), commandData, commandText, commandChat, commandMessage
    , parseCommands, commands, simpleCommands, pureCommand, simplePureCommand, actionCommand, simpleActionCommand
    ) where

import           ClassyPrelude
import           Control.Arrow ((+++), (>>>), (|||), arr, right, returnA)
import           Control.Lens (_Right, preview, to, view)
import           Control.Lens.TH (makeLenses)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent), onEventM)
import           Data.Attoparsec.Text (Parser, parseOnly)
import           Data.Monoid (First(First, getFirst))
import           Data.Proxy (Proxy(Proxy))
import qualified Data.Sequence as Seq
import           Data.Text (strip)
import           TextShow.TH (deriveTextShow)

import           Embot.Action (Action(SendMessage))
import           Embot.Slack ( RtmEvent, _RtmMessage
                             , RtmStartRp, rtmStartSelf
                             , Message, messageChat, messageText
                             , IM, Chat
                             , ID, isTypedID, unID
                             , selfID )
import           Embot.Types (EmbotWire)
import           Embot.Wire (filterMapE)

data Mention = Mention { _mentionText :: Text, _mentionChat :: ID Chat, _mentionMessage :: Message }
data Command a = Command { _commandData :: a, _commandText :: Text, _commandChat :: ID Chat, _commandMessage :: Message }

makeLenses ''Mention
makeLenses ''Command

deriveTextShow ''Mention
deriveTextShow ''Command

mentions :: Monad m => RtmStartRp -> EmbotWire m (Event RtmEvent) (Event Mention)
mentions rtmStartRp =
  filterMapE $ getFirst . foldMap (First <$>) [mentioned, imed]
  where
    inIM :: Message -> Bool
    inIM = fromMaybe False . map (isTypedID (Proxy :: Proxy IM)) . view messageChat

    mentionPrefix :: Text
    mentionPrefix = "<@" <> view (rtmStartSelf . selfID . to unID) rtmStartRp <> ">"

    mentioned :: RtmEvent -> Maybe Mention
    mentioned event = do
      m <- preview _RtmMessage event
      guard $ not . inIM $ m
      tail <- stripPrefix mentionPrefix . strip . view messageText $ m
      chatID <- view messageChat m
      Just $ Mention (strip tail) chatID m

    imed :: RtmEvent -> Maybe Mention
    imed event = do
      m <- preview _RtmMessage event
      guard $ inIM m
      let text = strip . view messageText $ m
      chatID <- view messageChat m
      Just $ Mention text chatID m

-- | Command parsing wire which generates events when Embot is mentioned or DMed/IMed some text which
--   is parsed by the trigger parser. Generates either @Left (Event error)@ or @Right (Event (Command a))@
--   when the trigger matches a mention, or @Left NoEvent@ (arbitrarily)
parseCommands
  :: Monad m
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser ()   -- ^ The trigger parser - if it parses successfully only then apply the
                  --   main parser. If this parser fails, then @NoEvent@ will be produced
  -> Parser a    -- ^ The main command parser. If this parser fails then @Left (Event _)@ will be produced.
  -> EmbotWire m (Event RtmEvent) (Either (Event (Mention, Text)) (Event (Command a)))
parseCommands rtmStartRp trigger parser =
    mentions rtmStartRp >>> arr f
  where
    f (Event mention) =
      let text = view mentionText mention in
        case parseOnly trigger text of
          Left _ -> Left NoEvent
          Right _ -> case parseOnly parser text of
            Left err -> Left . Event $ (mention, pack err)
            Right res -> Right . Event $ Command res text (view mentionChat mention) (view mentionMessage mention)
    f NoEvent = Left NoEvent

-- | Wire which either parses valid @Command@ values or generates failure responses
commands
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser ()   -- ^ The trigger parser - if it parses successfully only then apply the main parser. If this parser fails, then @NoEvent@ will be produced
  -> Parser a    -- ^ The main command parser. If this parser fails then @Event (Left _)@ will be produced.
  -> EmbotWire m (Event RtmEvent) (Either (Event (Seq Action)) (Event (Command a)))
commands rtmStartRp trigger parser =
  parseCommands rtmStartRp trigger parser >>> ((arr . map $ respondWithError) +++ returnA)
  where
    respondWithError (mention, failure) = Seq.singleton $ SendMessage (view mentionChat mention) failure

simpleCommands
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser a    -- ^ The command parser. If this parser fails then @NoEvent@ will be produced.
  -> EmbotWire m (Event RtmEvent) (Event (Command a))
simpleCommands rtmStartRp parser =
  commands rtmStartRp (parser *> pure ()) parser >>> arr (fromMaybe NoEvent . preview _Right)

-- | Command wire which applies some effect function in the case where a command is successfully parsed,
--   and reports the syntax error back to the sender in the case where the command is triggered
--   but not parsed correctly.
actionCommand
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser ()   -- ^ The trigger parser - if it parses successfully only then apply the main parser. If this parser fails, then @NoEvent@ will be produced
  -> Parser a    -- ^ The main command parser. If this parser fails then @Event (Left _)@ will be produced.
  -> (Command a -> m (Seq Action)) -- ^ The action to react to the command with
  -> EmbotWire m (Event RtmEvent) (Event (Seq Action))
actionCommand rtmStartRp trigger parser action =
  commands rtmStartRp trigger parser >>> right (onEventM action) >>> id ||| id

simpleActionCommand
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser a    -- ^ The main command parser. If this parser fails then @Event (Left _)@ will be produced.
  -> (Command a -> m (Seq Action)) -- ^ The action to react to the command with
  -> EmbotWire m (Event RtmEvent) (Event (Seq Action))
simpleActionCommand rtmStartRp parser action =
  actionCommand rtmStartRp (parser *> pure ()) parser action

-- | Command wire which applies some function in the case where a command is successfully parsed,
--   and reports the syntax error back to the sender in the case where the command is triggered
--   but not parsed correctly.
pureCommand
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser ()   -- ^ The trigger parser - if it parses successfully only then apply the main parser. If this parser fails, then @NoEvent@ will be produced
  -> Parser a    -- ^ The main command parser. If this parser fails then @Event (Left _)@ will be produced.
  -> (Command a -> Seq Action) -- ^ The function to react to the command with
  -> EmbotWire m (Event RtmEvent) (Event (Seq Action))
pureCommand rtmStartRp trigger parser =
  actionCommand rtmStartRp trigger parser . map pure

simplePureCommand
  :: (Applicative m, Monad m)
  => RtmStartRp  -- ^ The rtm.start reply
  -> Parser a    -- ^ The main command parser. If this parser fails then @Event (Left _)@ will be produced.
  -> (Command a -> Seq Action) -- ^ The function to react to the command with
  -> EmbotWire m (Event RtmEvent) (Event (Seq Action))
simplePureCommand rtmStartRp parser f =
  pureCommand rtmStartRp (parser *> pure ()) parser f
