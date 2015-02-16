module Embot.Plugins.Echo where

import           Control.Applicative (pure)
import           Control.Category ((.))
import           Control.Lens ((&), (^.), (^?), (.~))
import           Control.Monad ((>>=), fail, guard)
import           Control.Monad.Reader (asks)
import           Control.Monad.Writer (tell)
import           Data.Aeson ((.:))
import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Either (either)
import           Data.Function (($), flip)
import           Data.HList (HList(HCons))
import           Data.List (elem)
import           Data.Maybe (fromMaybe)
import           Embot.Action (Action(SendMessage))
import           Embot.Core (InterceptorInitializer, GlobalConfig, EnvElem(getEnv), chain, configRaw)
import           Embot.Event (_ReceivedMessage, eventConsumed, eventDetail)
import           Embot.SlackAPI (messageChannel)
import           Prelude (Bool(True), not)
import           Text.Show.Text (show)

echoInterceptor :: EnvElem es GlobalConfig => InterceptorInitializer (es :: [*]) () (is :: [*])
echoInterceptor (next, nextState) = do
    configJSON <- asks (configRaw . getEnv)
    conversations <- either fail pure . flip Aeson.parseEither configJSON $ \ o ->
        (o .: "echo") >>= Aeson.withObject "echo configuration" (.: "conversations")
    let intercept event =
            fromMaybe (chain next event) $ do
                guard . not $ event ^. eventConsumed
                message <- event ^? eventDetail . _ReceivedMessage
                conversationId <- message ^. messageChannel
                guard $ conversationId `elem` conversations
                pure $ do
                    tell [SendMessage conversationId $ show message]
                    chain next $ event & eventConsumed .~ True
    pure (intercept, () `HCons` nextState)

-- echoHandler :: forall sharedSV. EchoConfig -> Event -> PluginM () sharedSV Event
-- echoHandler config event = fromMaybe (pure event) $ do
--     guard . not $ event ^. eventConsumed
--     message <- event ^? eventDetail . _ReceivedMessage
--     conversationId <- message ^. messageChannel
--     guard $ conversationId `elem` (config ^. echoConversations)
--     tell [SendMessage conversationId $ show message]
--     pure $ event & eventConsumed .~ True
