{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Embot.Plugins where

import Control.Monad ((=<<))
import Control.Monad.Reader (runReaderT)
import Data.Function (($), flip)
import Data.HList.TIP (emptyTIP)
import Embot.Core (globalConfigEnv, nilInterceptorWithState)
import Embot.Plugins.Commands (basicCommands, basicCommandsEnv, commandsEnv)
-- import Embot.Plugins.Echo (echoInterceptor)
import Embot.Plugins.SlackInfo (slackInfoEnv)

initializeEnv globalConfig startRp =
    flip runReaderT globalConfig $
        (   basicCommandsEnv
        =<< commandsEnv
        =<< slackInfoEnv startRp
        =<< globalConfigEnv emptyTIP )

initializeInterceptors env =
    flip runReaderT env $
        -- ( echoInterceptor
        (   basicCommands
        $   nilInterceptorWithState )

