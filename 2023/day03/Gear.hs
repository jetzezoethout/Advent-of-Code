module Gear where

import           EngineIndicator (EngineIndicator (..))
import           EngineNumber    (EngineNumber (value), isAdjacentTo)

data Gear = Gear
  { value1 :: Int
  , value2 :: Int
  } deriving (Show)

getGear :: [EngineNumber] -> EngineIndicator -> Maybe Gear
getGear numbers EngineIndicator {..} =
  [ Gear {value1 = head adjacentNumbers, value2 = adjacentNumbers !! 1}
  | symbol == '*'
  , length adjacentNumbers == 2
  ]
  where
    adjacentNumbers = map value $ filter (location `isAdjacentTo`) numbers

ratio :: Gear -> Int
ratio Gear {..} = value1 * value2
