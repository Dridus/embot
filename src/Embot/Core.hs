{-# LANGUAGE OverlappingInstances #-}
module Embot.Core
    ( EmbotIO(unEmbotIO)
    , Env
    , EnvInitializer, globalConfigEnv
    , InterceptorInitializer
    , Interceptor, InterceptorWithState, chain, nilInterceptorWithState
    , InterceptorState(InterceptorState), environment, interceptorSV
    , InterceptorM
    , GlobalConfig, configRaw, apiToken, readGlobalConfig
    , EnvElem(envElem)
    , StateEnv(StateEnvOf, stateEnv), env
    ) where

import           Control.Applicative (Applicative, (<$>), (<*>), pure)
import           Control.Lens (Lens', (&), (.~), lens, view)
import           Control.Lens.TH (makeLenses)
import           Control.Monad (Monad, (>>=), fail)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Logger (MonadLogger, LoggingT, logError)
import           Control.Monad.Reader (ReaderT, ask)
import           Control.Monad.RWS.Strict (RWST(RWST, runRWST))
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import           Data.Either (Either(Left, Right))
import           Data.Function (($), (.), const, id)
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

-- |A `GlobalConfig` is the configuation file contents along with any parameters the core itself needs, like the Slack API key.
data GlobalConfig = GlobalConfig
    { configRaw :: Aeson.Object  -- ^AST of the configuration file data.
    , apiToken  :: Text          -- ^Slack API token in use.
    }

-- |The `EmbotIO` monad fuses Logging with IO and is newtyped with appropriate standalone deriving clauses so that additional transformers could be fused in without `lift` towers having to be made.
newtype EmbotIO a = EmbotIO { unEmbotIO :: LoggingT IO a }
deriving instance Applicative EmbotIO
deriving instance Functor EmbotIO
deriving instance Monad EmbotIO
deriving instance MonadIO EmbotIO
deriving instance MonadLogger EmbotIO

-- |An `Env` is a type indexed product (TIP) which contains the accumulated environment values produced by `EnvInitializer`s.
-- `es` is the TIP / HList type, and is list kinded (`[*]`) rather than normally kinded.
type Env (es :: [*]) = TIP es

-- |An `EnvInitializer` is an action which takes some existing shared environment state and adds something to it, possibly using `EmbotIO` to cause side effects.
-- Type `e` is the enviroment that will be added by this initializer, `es` is the already-initialized environment.
-- Can read the `GlobalConfig`.
type EnvInitializer e (es :: [*]) = Env es -> ReaderT GlobalConfig EmbotIO (TIP (e ': es))

-- |An `InterceptorInitializer` is an action which prepares an interceptor to be added to the interceptor chain.
-- An initializer receives some interceptor which should be called next with the event or events.
-- It also receives the state vector which is building up as type `is` and adds some state for the interceptor `i`
-- Can read the shared environment created in the previous stage of type `es`
type InterceptorInitializer (es :: [*]) i (is :: [*]) = InterceptorWithState es is -> ReaderT (Env es) EmbotIO (InterceptorWithState es (i ': is))

-- |An `Interceptor` is a function which executes in response to an event.
-- Typically chained by composed `InterceptorInitializers
type Interceptor (es :: [*]) (is :: [*]) = Event -> InterceptorM es is ()
-- |An `Interceptor` along with its state (`is`)
type InterceptorWithState (es :: [*]) (is :: [*]) = (Interceptor es is, HList is)

-- |The state available to an `InterceptorM` action.
data InterceptorState (es :: [*]) (is :: [*]) = InterceptorState
    { _environment   :: Env es     -- ^The environment, which is mutable.
    , _interceptorSV :: HList is   -- ^The interceptor state vector, typically the interceptor knows the type of the head of the list (called `i`) and will be able to modify it, but not the tail.
    }

-- |The `InterceptorM` monad is an `EmbotIO` with a `RWST` stacked on top to provide an outlet for `Action`s to take in response to the event as well as access to the shared environment `es` and interceptor state `is`.
type InterceptorM (es :: [*]) (is :: [*]) a = RWST () [Action] (InterceptorState es is) EmbotIO a

-- |Helper for "lifting" an interceptor action at the next link in the chain into the proper `RWST` for the current link in the chain.
-- Typically used in interceptors as `chain next` where `next` is the next interceptor to run.
-- Just applies the tail of the state vector to the action and then puts the dropped state back on afterwards.
chain :: forall (es :: [*]) (i :: *) (is :: [*]). Interceptor es is -> Interceptor es (i ': is)
chain next event =
    RWST $ \ () (InterceptorState es (i `HCons` is)) -> do
        (a, InterceptorState es' is', w) <- runRWST (next event) () (InterceptorState es is)
        pure (a, InterceptorState es' (i `HCons` is'), w)

-- |`EnvInitializer` which adds the `GlobalConfig` available "out of band" during `Env` initialization but not explicitly afterwards, allowing `InterceptorInitializer` and `Interceptor`s to read it.
globalConfigEnv :: (HTypeIndexed es, HOccursNot GlobalConfig es) => EnvInitializer GlobalConfig (es :: [*])
globalConfigEnv env' = do
    globalConfig <- ask
    pure . mkTIP . HCons globalConfig . unTIP $ env'

-- |The nil interceptor which does nothing and lies at the end of the interceptor chain.
nilInterceptorWithState :: InterceptorWithState (es :: [*]) '[]
nilInterceptorWithState = (const $ pure (), HNil)

-- |`EnvElem` can be deduced when a particular environment `e` has been added to the environment `es`.
class EnvElem (es :: [*]) e where
    -- |`env` is a polymorphic lens that accesses a particular state indexed by its type `e` from the total environment `es`
    envElem :: Lens' (Env es) e

-- |`StateEnv` connects `MonadState` actions to the type indexed environment provided by `EnvElem` by providing a lens from the state `s` to the `Env es`
class StateEnv s where
    -- |`StateEnvOf s` is the type of the environment stored in `s`
    type StateEnvOf s :: [*]
    -- |`stateEnv s` is the environment stored in `s`
    stateEnv :: Lens' s (Env (StateEnvOf s))

makeLenses ''InterceptorState

instance FromJSON GlobalConfig where
    parseJSON = withObject "global configuration" $ \ o -> GlobalConfig
        <$> pure o
        <*> o .: "api-token"

-- |Parse the global config file, failing if it can't be read
readGlobalConfig :: EmbotIO GlobalConfig
readGlobalConfig = liftIO (Yaml.decodeFileEither "embot.yml") >>= \ case
    Left failure -> do
        $logError $ "Failed to read global config file from embot.yml: " <> (pack . show $ failure)
        fail "Failed to read configuration file"
    Right config -> pure config

instance HTypeIndexed es => EnvElem (e ': es) e where
    envElem = lens (hHead . unTIP) (\ (unTIP -> _ `HCons` tail) e -> TIP (e `HCons` tail))

instance (HOccursNot f es, HTypeIndexed es, EnvElem es e) => EnvElem (f ': es) e where
    envElem = lens (view env . onTIP hTail) (\ tip@(unTIP -> f `HCons` _) e -> onTIP (f `HCons`) (onTIP hTail tip & env .~ e))

instance StateEnv (InterceptorState es is) where
    type StateEnvOf (InterceptorState es is) = es
    stateEnv = environment

instance StateEnv (Env es) where
    type StateEnvOf (Env es) = es
    stateEnv = lens id const

-- |Lens from some state `s` which has a shared environment available to a piece of that shared environment of type `e`.
-- Used in `InterceptorM` actions to get access to the shared environment.
env :: (StateEnv s, EnvElem (StateEnvOf s) e) => Lens' s e
env = stateEnv . envElem
