module Slope where

import           Coordinate (Coordinate (..))

data Slope = Slope
  { right :: Int
  , down  :: Int
  }

atTime :: Slope -> Int -> Coordinate
Slope {..} `atTime` t = Coordinate {row = down * t, column = right * t}
