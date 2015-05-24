module Main where

import ClassyPrelude (($))
import Embot (embot, embotDefaultConfig)
import Embot.Examples.Echo (echoLogic)
import System.IO (IO)

main :: IO ()
main = embot config
  where
    config = embotDefaultConfig "xoxb-3212154165-yKrZX5u6Cnfkcy5dYOYfLBhO" ($ echoLogic)
