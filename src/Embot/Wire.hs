module Embot.Wire
  ( accumEM
  , filterSameE
  ) where

import           ClassyPrelude
import           Control.Wire (Wire, mkGenN, mkSFN)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent), event)

accumEM
  :: Monad m
  => (b -> a -> m b)
  -> b
  -> Wire s e m (Event a) (Event b)
accumEM k = loop
  where
    loop x' =
      mkGenN $
       event (pure (Right NoEvent, loop x'))
             (\ y -> k x' y >>= \ x -> pure (Right $ Event x, loop x))

filterSameE :: Eq a => Wire s e m (Event a) (Event a)
filterSameE = storeAndForward
  where
    storeAndForward =
      mkSFN $
        event (NoEvent, storeAndForward)
              (\ a -> (Event a, holdUntilChange a))
    holdUntilChange a =
      mkSFN $
        event (NoEvent, holdUntilChange a)
              (\ a' -> if a /= a' then (Event a', holdUntilChange a')
                                  else (NoEvent,  holdUntilChange a))
