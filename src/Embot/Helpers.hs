module Embot.Helpers where

import           ClassyPrelude (map)
import           Control.Applicative (pure)
import           Control.Category ((.))
import           Control.Lens ((^.), (^?))
import           Control.Monad.Writer.Class (tell)
import           Data.Bool (Bool(False))
import           Data.Function (($))
import           Data.Monoid ((<>))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Embot.Action (Action(SendMessage))
import           Embot.Core (InterceptorM)
import           Embot.Event (Event, eventDetail, _ReceivedMessage)
import           Embot.SlackAPI (ID, messageConversation, messageText, User)
import           Text.Show.Text (show)

mentionedInEvent :: ID User -> Event -> Bool
uid `mentionedInEvent` event =
    fromMaybe False $ do
        text <- event ^? eventDetail . _ReceivedMessage . messageText
        pure $ ("<@" <> show uid <> ">") `T.isInfixOf` text

replyTo :: Event -> [Text] -> InterceptorM es is ()
replyTo event messages = fromMaybe (pure ()) $ do
    message <- event ^? eventDetail . _ReceivedMessage
    conversationId <- message ^. messageConversation
    pure . tell $ map (SendMessage conversationId) messages
