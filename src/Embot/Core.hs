{-# LANGUAGE OverlappingInstances #-}
module Embot.Core
    ( EmbotIO(unEmbotIO)
    , EnvInitializer, globalConfigEnv
    , InterceptorInitializer
    , Interceptor, InterceptorWithState, chain, nilInterceptorWithState
    , InterceptorM
    , GlobalConfig, configRaw, apiToken, readGlobalConfig
    , EnvElem, getEnv, mapEnv
    -- , getEnv, replaceSharedState, modifySharedState
    ) where

import           Control.Applicative (Applicative, (<$>), (<*>), pure)
import           Control.Monad (Monad, (>>=), fail)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger, LoggingT, logError)
import           Control.Monad.Reader (ReaderT, ask)
import           Control.Monad.RWS.Strict (RWST(RWST, runRWST))
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import           Data.Either (Either(Left, Right))
import           Data.Function (($), (.), const)
import           Data.Functor (Functor)
import           Data.HList (HList(HCons, HNil), HOccursNot, hHead, hTail)
import           Data.HList.TIP (HTypeIndexed, TIP(TIP, unTIP), mkTIP, onTIP)
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Yaml as Yaml
import           Embot.Action (Action)
import           Embot.Event (Event)
import           System.IO (IO)
import           Text.Show (show)

data GlobalConfig = GlobalConfig
    { configRaw :: Aeson.Object
    , apiToken  :: Text
    }

newtype EmbotIO a = EmbotIO { unEmbotIO :: LoggingT IO a }
deriving instance Applicative EmbotIO
deriving instance Functor EmbotIO
deriving instance Monad EmbotIO
deriving instance MonadIO EmbotIO
deriving instance MonadLogger EmbotIO

type EnvInitializer e (es :: [*]) = TIP es -> ReaderT GlobalConfig EmbotIO (TIP (e ': es))

type InterceptorInitializer (es :: [*]) i (is :: [*]) = InterceptorWithState es is -> ReaderT (TIP es) EmbotIO (InterceptorWithState es (i ': is))

type Interceptor (es :: [*]) (is :: [*]) = Event -> InterceptorM es is ()
type InterceptorWithState (es :: [*]) (is :: [*]) = (Interceptor es is, HList is)

type InterceptorM (es :: [*]) (is :: [*]) a = RWST () [Action] (TIP es, HList is) EmbotIO a

chain :: forall (es :: [*]) (i :: *) (is :: [*]). Interceptor es is -> Interceptor es (i ': is)
chain next event =
    RWST $ \ () (es, i `HCons` is) -> do
        (a, (es', is'), w) <- runRWST (next event) () (es, is)
        pure (a, (es', i `HCons` is'), w)

globalConfigEnv :: (HTypeIndexed es, HOccursNot GlobalConfig es) => EnvInitializer GlobalConfig (es :: [*])
globalConfigEnv env' = do
    globalConfig <- ask
    pure . mkTIP . HCons globalConfig . unTIP $ env'

nilInterceptorWithState :: InterceptorWithState (es :: [*]) '[]
nilInterceptorWithState = (const $ pure (), HNil)

class EnvElem (sharedSV :: [*]) sharedS where
    getEnv :: TIP sharedSV -> sharedS
    mapEnv :: (sharedS -> sharedS) -> TIP sharedSV -> TIP sharedSV

-- class HasSharedSV a where
--     type SharedSVOf a :: [*]
--     sharedSVOf :: Lens' a (TIP (SharedSVOf a))

instance FromJSON GlobalConfig where
    parseJSON = withObject "global configuration" $ \ o -> GlobalConfig
        <$> pure o
        <*> o .: "api-token"

readGlobalConfig :: EmbotIO GlobalConfig
readGlobalConfig = liftIO (Yaml.decodeFileEither "embot.yml") >>= \ case
    Left failure -> do
        $logError $ "Failed to read global config file from embot.yml: " <> (pack . show $ failure)
        fail "Failed to read configuration file"
    Right config -> pure config

-- makeLenses ''Env
-- makeLenses ''DynamicEnv
-- makeLenses ''RuntimeConfiguration
-- makeLenses ''PluginEnv

-- initialRuntimeConfiguration :: RuntimeConfiguration '[] '[]
-- initialRuntimeConfiguration =
--     RuntimeConfiguration HNil (DynamicEnv emptyTIP)

-- statelessPlugin :: (Event -> PluginM () sharedSV Event) -> Plugin () privateSV sharedSV sharedSV
-- statelessPlugin = plugin ()

-- plugin :: privateS -> (Event -> PluginM privateS sharedSV Event) -> Plugin privateS privateSV sharedSV sharedSV
-- plugin privateS handler =
--     eventHandler %~ HCons (privateS, handler)

-- pluginWithSharedState :: HTypeIndexed (sharedS ': sharedSV) => privateS -> sharedS -> (Event -> PluginM privateS (sharedS ': sharedSV) Event) -> Plugin privateS privateSV sharedSV (sharedS ': sharedSV)
-- pluginWithSharedState privateS sharedS handler (RuntimeConfiguration { _dynamicEnv = DynamicEnv { .. }, _eventHandler }) = RuntimeConfiguration
--     ( eventHandler             %~ HCons (privateS, handler)
--     . dynamicEnv . sharedState %~ onTIP (HCons sharedS)
--     )

instance HTypeIndexed restOfState => EnvElem (state ': restOfState) state where
    getEnv = hHead . unTIP
    mapEnv f (TIP (HCons state tail)) = TIP (HCons (f state) tail)

instance (HTypeIndexed restOfState, EnvElem restOfState state) => EnvElem (unrelatedState ': restOfState) state where
    getEnv = getEnv . onTIP hTail
    mapEnv f (TIP (hd `HCons` tl)) = TIP $ hd `HCons` unTIP (mapEnv f (TIP tl))

-- instance HasSharedSV (DynamicEnv sv) where
--     type SharedSVOf (DynamicEnv sv) = sv
--     sharedSVOf = sharedState

-- instance HasSharedSV (PluginEnv p sv) where
--     type SharedSVOf (PluginEnv p sv) = sv
--     sharedSVOf = pluginDynamicEnv . sharedState

-- viewSharedState :: (EnvElem sharedSV sharedS, MonadState (DynamicEnv sharedSV) m) => m sharedS
-- viewSharedState = gets $ getEnv . view sharedSVOf

-- replaceSharedState :: (EnvElem sharedSV sharedS, MonadState (DynamicEnv sharedSV) m) => sharedS -> m ()
-- replaceSharedState state = modify $ sharedSVOf %~ mapEnv (const state)

-- modifySharedState :: (EnvElem sharedSV sharedS, MonadState (DynamicEnv sharedSV) m) => (sharedS -> sharedS) -> m ()
-- modifySharedState f = modify $ sharedSVOf %~ mapEnv f

