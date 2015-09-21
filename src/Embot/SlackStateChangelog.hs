module Embot.SlackStateChangelog (slackStateLogger) where

import           ClassyPrelude
import           Control.Arrow ((<<<), arr)
import           Control.Lens (Getter, view, to)
import           Control.Monad (mfilter)
import           Control.Monad.Logger (logInfo)
import           Control.Wire (Event)
import           Control.Wire.Unsafe.Event (onEventM)
import           Data.Algorithm.Diff (Diff(First, Second, Both), getDiff)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           TextShow (TextShow, showt)

import Embot.Slack (ID, RtmEvent, unID, channelName, groupName, imUser, idedName, userName)
import Embot.SlackState (SlackState, channels, groups, ims, users)
import Embot.Types (EmbotWire, MinimalEmbotMonad)
import Embot.Wire (diffByE)

slackStateLogger :: MinimalEmbotMonad m => SlackState m -> EmbotWire m (Event RtmEvent) (Event ())
slackStateLogger slackState =
  proc event -> do
    onEventM (logDiff channelName "Channels changed") <<< diffByE ((==) `on` fst) <<< arr (map Map.toAscList) <<< view channels slackState -< event
    onEventM (logDiff groupName   "Groups changed")   <<< diffByE ((==) `on` fst) <<< arr (map Map.toAscList) <<< view groups   slackState -< event
    onEventM (logDiff imLabel     "IMs changed")      <<< diffByE ((==) `on` fst) <<< arr (map Map.toAscList) <<< view ims      slackState -< event
    onEventM (logDiff userName    "Users changed")    <<< diffByE ((==) `on` fst) <<< arr (map Map.toAscList) <<< view users    slackState -< event
    arr . map . const $ () -< event
  where
    imLabel = imUser . to unID

logDiff :: (MinimalEmbotMonad m, Eq a, TextShow a) => Getter a Text -> Text -> [Diff (ID k, a)] -> m ()
logDiff name heading diffs =
  when (not . null $ diffs) $ do
    $logInfo heading
    forM_ diffs $ \ case
      First (i, a)  -> $logInfo $ "- " ++ prefix i a
      Second (i, a) -> $logInfo $ "+ " ++ prefix i a
      Both (i, a) (_, a') | a /= a' -> do
        let subdiff = toList . trimEnds . Seq.fromList
                    $ getDiff (unpack $ showt a) (unpack $ showt a')

            context = 20

            trimEnds = trimL . trimR
            trimL s = maybe s ((Both '…' '…' Seq.<|) . snd . flip Seq.splitAt s . subtract context)
                    . mfilter (>= context) . Seq.findIndexL (not . bothEq)
                    $ s
            trimR s = maybe s ((Seq.|> Both '…' '…') . fst . flip Seq.splitAt s . (length s -) . subtract context)
                    . mfilter (>= context) . map (length s -) . Seq.findIndexR (not . bothEq)
                    $ s

            pre = flip concatMap subdiff $ \ case
              First ch  -> [ch]
              Both ch _ -> [ch]
              _         -> []

            post = flip concatMap subdiff $ \ case
              Second ch -> [ch]
              Both _ ch -> [ch]
              _         -> []

        $logInfo $ "/ " ++ prefix i a' ++ ": " ++ pack pre ++ " → " ++ pack post

      _ -> pure ()


  where
    prefix i = idedName name (to . const $ i)

    bothEq (Both a b) = a == b
    bothEq _          = False
