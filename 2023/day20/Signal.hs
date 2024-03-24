module Signal where

import           Data.Text (Text)

data Pulse
  = Low
  | High
  deriving (Eq, Show)

data Signal = Signal
  { pulse  :: Pulse
  , source :: Text
  , target :: Text
  } deriving (Show)

isCycleIndicator :: Text -> Signal -> Bool
isCycleIndicator cycleTarget Signal {..} =
  pulse == High && source == cycleTarget
