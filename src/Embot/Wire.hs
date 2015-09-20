module Embot.Wire
  ( accumEM
  , filterMapE, filterSameE
  , priorAndCurrentE, priorAndCurrent1E
  , diffE, diffByE, groupedDiffE, groupedDiffByE
  ) where

import           ClassyPrelude
import           Control.Arrow ((>>>), arr)
import           Control.Wire (Wire, mkGenN, mkSFN)
import           Control.Wire.Unsafe.Event (Event(Event, NoEvent), event)
import           Data.Algorithm.Diff (Diff, getDiffBy, getGroupedDiffBy)

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

filterMapE :: Monad m => (a -> Maybe b) -> Wire s e m (Event a) (Event b)
filterMapE f =
  arr $ event NoEvent (maybe NoEvent Event . f)

filterSameE :: (Monad m, Eq a) => Wire s e m (Event a) (Event a)
filterSameE =
  priorAndCurrentE >>> filterMapE f
  where
    f (Just a, a') | a == a' = Nothing
    f (_,      a')           = Just a'

priorAndCurrentE :: Wire s e m (Event a) (Event (Maybe a, a))
priorAndCurrentE = loop Nothing
  where
    loop aMay =
      mkSFN $
        event (NoEvent, loop aMay)
              (\ a' -> (Event (aMay, a'), loop (Just a')))

priorAndCurrent1E :: Monad m => Wire s e m (Event a) (Event (a, a))
priorAndCurrent1E =
  priorAndCurrentE >>> filterMapE f
  where
    f (Just a, a') = Just (a, a')
    f _            = Nothing

diffE :: (Monad m, Eq a) => Wire s e m (Event [a]) (Event [Diff a])
diffE =
  diffByE (==)

diffByE :: Monad m => (a -> a -> Bool) -> Wire s e m (Event [a]) (Event [Diff a])
diffByE f =
  priorAndCurrentE >>> (arr . map . first $ fromMaybe []) >>> (arr . map . uncurry $ getDiffBy f)

groupedDiffE :: (Monad m, Eq a) => Wire s e m (Event [a]) (Event [Diff [a]])
groupedDiffE =
  groupedDiffByE (==)

groupedDiffByE :: Monad m => (a -> a -> Bool) -> Wire s e m (Event [a]) (Event [Diff [a]])
groupedDiffByE f =
  priorAndCurrentE >>> (arr . map . first $ fromMaybe []) >>> (arr . map . uncurry $ getGroupedDiffBy f)
