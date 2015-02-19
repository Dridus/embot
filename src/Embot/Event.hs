module Embot.Event where

import           Control.Lens.TH (makeLenses, makePrisms)
import           Data.Bool (Bool)
import           Embot.SlackAPI (Message)
import           Text.Show.Text.TH (deriveShow)

data Event = Event
    { _eventConsumed :: Bool
    , _eventDetail   :: EventDetail
    }

data EventDetail
    = ReceivedMessage Message

deriveShow ''Event
deriveShow ''EventDetail
makeLenses ''Event
makePrisms ''EventDetail

