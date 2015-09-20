module Embot.Utils (applyWhen) where

import ClassyPrelude

applyWhen :: (a -> Bool) -> (a -> a) -> (a -> a)
applyWhen p f a | p a       = f a
                | otherwise =   a
