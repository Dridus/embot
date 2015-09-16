{-# OPTIONS_GHC -fno-warn-orphans #-}
module Embot.TextShowOrphans where

import Control.Category ((.))
import Control.Wire.Unsafe.Event (Event, event)
import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HM
import Data.Monoid ((<>))
import Text.Show.Text (FromStringShow(FromStringShow), Show(showbPrec))

-- powerful orphan instance time!
instance Show Value where
  showbPrec prec = showbPrec prec . FromStringShow
instance (Show k, Show v) => Show (HM.HashMap k v) where
  showbPrec prec = ("HM.fromList " <>) . showbPrec prec . HM.toList
instance Show a => Show (Event a) where
  showbPrec prec = event "NoEvent" (("Event " <>) . showbPrec prec)
