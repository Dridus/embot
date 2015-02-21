module Embot.Plugins.Commands where

import           ClassyPrelude (error, map)
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
import Embot.SlackAPI (ID(unID), asIMID, User, selfId, messageChat, messageText)

data Command = Command
    { _commandSyntax      :: Text
    , _commandPattern     :: Text
    , _commandAccessible  :: ID User -> EmbotIO Bool
    , _commandSummary     :: Text
    }

type Commands = Set Command

makeLenses ''Command

instance Eq Command where
    (view commandSyntax -> a) == (view commandSyntax -> b) =
        a == b
instance Ord Command where
    (view commandSyntax -> a) `compare` (view commandSyntax -> b) =
        (CI.mk a) `compare` (CI.mk b)

wordsToPattern :: Text -> Text
wordsToPattern =
      T.intercalate "\\s+"
    . map (fromMaybe (error "no match group zero?") . ICU.group 0)
    . ICU.findAll "\\S+"

alwaysAccessible :: ID User -> EmbotIO Bool
alwaysAccessible = const $ pure True

simpleCommand :: Text -> Text -> Command
simpleCommand words summary = Command
    { _commandSyntax     =                words
    , _commandPattern    = wordsToPattern words
    , _commandAccessible = alwaysAccessible
    , _commandSummary    = summary
    }

devCommand :: Text -> Text -> Command
devCommand words summary = Command
    { _commandSyntax     =                  "dev " <> words
    , _commandPattern    = wordsToPattern $ "dev " <> words
    , _commandAccessible = alwaysAccessible -- FIXME toggle accessibility with some per-user switch to hide away maintenance commands
    , _commandSummary    = summary
    }

commandsEnv :: NotEnvElem Commands es => EnvInitializer es (Commands ': es)
commandsEnv = pure . onTIP (HCons Set.empty)

addCommand :: EnvElem Commands es => Command -> Env (es :: [*]) -> Env es
addCommand cmd = envElem %~ Set.insert cmd

helpCommand :: Command
helpCommand = simpleCommand "help" "Show help for commands."

basicCommandsEnv :: EnvElem Commands es => EnvInitializer es es
basicCommandsEnv =
    pure . addCommand helpCommand

basicCommands :: (EnvElem Commands es, EnvElem SlackInfo es) => InterceptorInitializer es is is
basicCommands (next, nextState) = pure $ (onCommand helpCommand handleHelp next, nextState)
  where
    handleHelp event = do
        sid                <- use $ env . infoSelf . selfId
        commands           <- uses env Set.toAscList
        accessibleCommands <- filterM (lift . ($ sid) . view commandAccessible) commands
        replyTo event $ accessibleCommands >>= \ command ->
            [command ^. commandSyntax, "    " <> command ^. commandSummary]
        next $ event & eventConsumed .~ True

onCommand :: (EnvElem Commands es, EnvElem SlackInfo es) => Command -> Interceptor es is -> Interceptor es is -> Interceptor es is
onCommand command =
    let mentionPattern = ICU.regex [ICU.CaseInsensitive] ("<@(U[0-9A-Z]+)>[^a-zA-Z]*(?:" <> command ^. commandPattern <> ")\\b")
        dmPattern      = ICU.regex [ICU.CaseInsensitive] ("^\\s*(?:" <> command ^. commandPattern <> ")\\b")
    in \ onMatch orElse ->
            \ event -> do
                sid <- use $ env . infoSelf . selfId
                fromMaybe (orElse event) $ do
                    guard . not $ event ^. eventConsumed
                    text            <- event ^? eventDetail . _ReceivedMessage . messageText
                    chatMay <- event ^? eventDetail . _ReceivedMessage . messageChat
                    chat    <- chatMay
                    case asIMID chat of
                        Just _ -> void $ ICU.find dmPattern text
                        Nothing -> do
                            match <- ICU.find mentionPattern text
                            mentioned <- ICU.group 1 match
                            guard $ mentioned == unID sid
                    pure $ onMatch event


