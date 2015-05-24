module Embot.Types
    ( LoggingIO(..)
    , LogFunction
    , EmbotLogic
    , EmbotConfig(..), configurationError, apiToken, setupLogging, withEnvironment
    , MinimalEmbotMonad
    ) where

import Control.Applicative (Applicative)
import Control.Lens.TH (makeLenses)
import Control.Monad (Monad)
import Control.Monad.Logger (LoggingT, Loc, LogLevel, LogSource, MonadLogger)
import Control.Monad.Trans (MonadIO)
import Control.Wire (Event, Timed, Wire)
import Data.Functor (Functor)
import Data.Maybe (Maybe)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import System.IO (IO)
import System.Log.FastLogger (LogStr)

import Embot.Action (Action)
import Embot.Slack (RtmEvent)

-- |The `LoggingIO` monad fuses Logging with IO and is newtyped with appropriate standalone deriving clauses so that additional transformers could be fused in without `lift` towers having to be made.
newtype LoggingIO a = LoggingIO { unLoggingIO :: LoggingT IO a }
deriving instance Applicative LoggingIO
deriving instance Functor     LoggingIO
deriving instance Monad       LoggingIO
deriving instance MonadIO     LoggingIO
deriving instance MonadLogger LoggingIO

type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type EmbotLogic m = Wire (Timed NominalDiffTime ()) () m (Event RtmEvent) (Event (Seq Action))

data EmbotConfig m = EmbotConfig
    { _configurationError :: Maybe Text
    , _apiToken           :: Text
    , _setupLogging       :: IO LogFunction
    , _withEnvironment    :: forall a. (EmbotLogic m -> m a) -> LoggingIO a
    }

type MinimalEmbotMonad m = (Applicative m, Monad m, MonadIO m, MonadLogger m)

makeLenses ''EmbotConfig
