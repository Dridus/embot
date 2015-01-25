module Embot.Env where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens.TH (makeLenses)
import           Control.Monad ((>>=), fail)
import qualified Data.Aeson as Aeson
import           Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import           Data.Either (either)
import           Data.Function (($), (.))
import           Data.Text (Text)
import qualified Data.Yaml as Yaml
import           System.IO (IO)
import           Text.Show (show)

data Env = Env
    { _envGlobalConfig :: GlobalConfig
    }

data GlobalConfig = GlobalConfig
    { _configRaw :: Aeson.Object
    , _apiToken :: Text
    }

makeLenses ''Env
makeLenses ''GlobalConfig

instance FromJSON GlobalConfig where
    parseJSON = withObject "global configuration" $ \ o -> GlobalConfig
        <$> pure o
        <*> o .: "api-token"

readGlobalConfig :: IO GlobalConfig
readGlobalConfig = Yaml.decodeFileEither "embot.yml" >>= either (fail . show) pure
