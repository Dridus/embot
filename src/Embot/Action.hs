module Embot.Action where

import ClassyPrelude
import Control.Lens.TH (makePrisms)
import Embot.Slack (ID, Chat)
import TextShow.TH (deriveTextShow)

data Action
  = SendMessage (ID Chat) Text

deriveTextShow ''Action
makePrisms ''Action
