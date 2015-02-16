{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Embot.Plugins where

import Control.Monad.Reader (runReaderT)
import Data.Function (($), flip)
import Data.HList.TIP (emptyTIP)
import Embot.Core (globalConfigEnv, nilInterceptorWithState)
import Embot.Plugins.Echo (echoInterceptor)

initializeEnv globalConfig =
    flip runReaderT globalConfig $
        ( globalConfigEnv
        $ emptyTIP )

initializeInterceptors env =
    flip runReaderT env $
        ( echoInterceptor
        $ nilInterceptorWithState )

