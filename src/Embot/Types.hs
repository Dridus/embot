module Embot.Types
  ( LoggingIO(..)
  , LogFunction
  , EmbotWire, EmbotLogic
  , EmbotConfig(..), configurationError, apiToken, setupLogging, withEnvironment
  , MinimalEmbotMonad
  ) where

import ClassyPrelude
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (LoggingT, Loc, LogLevel, LogSource, MonadLogger)
import Control.Wire (Event, Timed, Wire)
import Data.Time.Clock (NominalDiffTime)
import System.Log.FastLogger (LogStr)

import Embot.Action (Action)
import Embot.Slack (RtmEvent, RtmStartRp)

-- |The `LoggingIO` monad fuses Logging with IO and is newtyped with appropriate standalone deriving clauses so that additional transformers could be fused in without `lift` towers having to be made.
newtype LoggingIO a = LoggingIO { unLoggingIO :: LoggingT IO a }
deriving instance Applicative LoggingIO
deriving instance Functor     LoggingIO
deriving instance Monad       LoggingIO
deriving instance MonadIO     LoggingIO
deriving instance MonadLogger LoggingIO

type LogFunction = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

type EmbotWire m = Wire (Timed NominalDiffTime ()) () m

type EmbotLogic m = EmbotWire m (Event RtmEvent) (Event (Seq Action))

data EmbotConfig m = EmbotConfig
  { _configurationError :: Maybe Text
  , _apiToken           :: Text
  , _setupLogging       :: IO LogFunction
  , _withEnvironment    :: forall a. RtmStartRp -> (EmbotLogic m -> m a) -> LoggingIO a }

type MinimalEmbotMonad m = (Applicative m, Monad m, MonadIO m, MonadLogger m)

makeLenses ''EmbotConfig
