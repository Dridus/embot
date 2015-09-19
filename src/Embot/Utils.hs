module Embot.Utils (when) where

import ClassyPrelude

when :: (a -> Bool) -> (a -> a) -> (a -> a)
when p f a | p a       = f a
           | otherwise =   a
