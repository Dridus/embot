module Embot.Plugins.Echo where

import           Control.Applicative (pure)
import           Control.Category ((.))
import           Control.Lens ((&), (^.), (^?), (.~), views)
import           Control.Monad ((>>=), fail, guard)
import           Control.Monad.Writer (tell)
import           Data.Aeson ((.:))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Either (either)
import           Data.Function (($), flip)
import           Data.List (elem)
import           Data.Maybe (fromMaybe)
import           Embot.Action (Action(SendMessage))
import           Embot.Core (InterceptorInitializer, GlobalConfig, EnvElem, configRaw, env)
import           Embot.Event (_ReceivedMessage, eventConsumed, eventDetail)
import           Embot.SlackAPI (messageConversation)
import           Prelude (Bool(True), not)
import           Text.Show.Text (show)

echoInterceptor :: EnvElem GlobalConfig es => InterceptorInitializer es is is
echoInterceptor (next, nextState) = do
    configJSON <- views env configRaw
    conversations <- either fail pure . flip Aeson.parseEither configJSON $ \ o ->
        (o .: "echo") >>= Aeson.withObject "echo configuration" (.: "conversations")
    let intercept event =
            fromMaybe (next event) $ do
                guard . not $ event ^. eventConsumed
                message <- event ^? eventDetail . _ReceivedMessage
                conversationId <- message ^. messageConversation
                guard $ conversationId `elem` conversations
                pure $ do
                    tell [SendMessage conversationId $ show message]
                    next $ event & eventConsumed .~ True
    pure (intercept, nextState)
