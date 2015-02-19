module Embot.Plugins.Commands where

import           ClassyPrelude (map)
import           Control.Applicative (pure)
import           Control.Category ((.))
import           Control.Lens ((&), (^.), (^?), (.~), (%~), use, uses, view)
import           Control.Lens.TH (makeLenses)
import           Control.Monad ((>>=), filterM, guard, void)
import           Control.Monad.Trans (lift)
import           Data.Bool (Bool(True), not)
import qualified Data.CaseInsensitive as CI
import           Data.Eq (Eq, (==))
import           Data.Function (($), const)
import           Data.HList (HList(HCons), onTIP)
import           Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import           Data.Monoid ((<>))
import           Data.Ord (Ord(compare))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU as ICU

import Embot.Core (EmbotIO, Env, EnvElem(envElem), EnvInitializer, Interceptor, InterceptorInitializer, NotEnvElem, env)
import Embot.Event (eventConsumed, eventDetail, _ReceivedMessage)
import Embot.Helpers (replyTo)
import Embot.Plugins.SlackInfo (SlackInfo, infoSelf)
import Embot.SlackAPI (ID(unID), asIMID, User, selfId, messageConversation, messageText)

data Command = Command
    { _commandPrimary     :: Text
    , _commandSecondaries :: [Text]
    , _commandAccessible  :: ID User -> EmbotIO Bool
    , _commandSummary     :: Text
    }

type Commands = Set Command

makeLenses ''Command

instance Eq Command where
    (view commandPrimary -> a) == (view commandPrimary -> b) =
        a == b
instance Ord Command where
    (view commandPrimary -> a) `compare` (view commandPrimary -> b) =
        (CI.mk a) `compare` (CI.mk b)

commandsEnv :: NotEnvElem Commands es => EnvInitializer es (Commands ': es)
commandsEnv = pure . onTIP (HCons Set.empty)

addCommand :: EnvElem Commands es => Command -> Env (es :: [*]) -> Env es
addCommand cmd = envElem %~ Set.insert cmd

helpCommand :: Command
helpCommand = Command
    { _commandPrimary = "help"
    , _commandSecondaries = []
    , _commandAccessible = const $ pure True
    , _commandSummary = "Show help for commands."
    }

basicCommandsEnv :: EnvElem Commands es => EnvInitializer es es
basicCommandsEnv =
    pure . addCommand helpCommand

basicCommands :: (EnvElem Commands es, EnvElem SlackInfo es) => InterceptorInitializer es is is
basicCommands (next, nextState) = pure $ (onCommand helpCommand handleHelp next, nextState)
  where
    handleHelp event = do
        sid <- use $ env . infoSelf . selfId
        commands <- uses env Set.toAscList
        accessibleCommands <- filterM (lift . ($ sid) . view commandAccessible) commands
        replyTo event $ accessibleCommands >>= \ command ->
            let firstLine = T.justifyLeft 20 ' ' (command ^. commandPrimary) <> " " <> (command ^. commandSummary)
                tailLines = case command ^. commandSecondaries of
                     [] -> []
                     words -> [T.intercalate ", " words]
            in firstLine : tailLines
        next $ event & eventConsumed .~ True

onCommand :: (EnvElem Commands es, EnvElem SlackInfo es) => Command -> Interceptor es is -> Interceptor es is -> Interceptor es is
onCommand command =
    let words          = (command ^. commandPrimary) : (command ^. commandSecondaries)
        wordsExpr      = T.intercalate "|" $ map (\ word -> "\\Q" <> word <> "\\E") words
        mentionPattern = ICU.regex [ICU.CaseInsensitive] ("<@(U[0-9A-Z]+)>[^a-zA-Z]*(?:" <> wordsExpr <> ")\\b")
        dmPattern      = ICU.regex [ICU.CaseInsensitive] ("^\\s*(?:" <> wordsExpr <> ")\\b")
    in \ onMatch orElse ->
            \ event -> do
                sid <- use $ env . infoSelf . selfId
                fromMaybe (orElse event) $ do
                    guard . not $ event ^. eventConsumed
                    text            <- event ^? eventDetail . _ReceivedMessage . messageText
                    conversationMay <- event ^? eventDetail . _ReceivedMessage . messageConversation
                    conversation    <- conversationMay
                    case asIMID conversation of
                        Just _ -> void $ ICU.find dmPattern text
                        Nothing -> do
                            match <- ICU.find mentionPattern text
                            mentioned <- ICU.group 1 match
                            guard $ mentioned == unID sid
                    pure $ onMatch event


