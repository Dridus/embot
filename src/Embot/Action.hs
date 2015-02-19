module Embot.Action where

import Control.Lens.TH (makePrisms)
import Data.Text (Text)
import Embot.SlackAPI (ID, Conversation)
import Text.Show.Text.TH (deriveShow)

data Action
    = SendMessage (ID Conversation) Text

deriveShow ''Action
makePrisms ''Action
