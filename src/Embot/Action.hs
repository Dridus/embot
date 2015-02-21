module Embot.Action where

import Control.Lens.TH (makePrisms)
import Data.Text (Text)
import Embot.SlackAPI (ID, Chat)
import Text.Show.Text.TH (deriveShow)

data Action
    = SendMessage (ID Chat) Text

deriveShow ''Action
makePrisms ''Action
