module Gear where

import           Control.Monad   (guard)
import           EngineIndicator (EngineIndicator (..))
import           EngineNumber    (EngineNumber (value), isAdjacentTo)

data Gear = Gear
  { value1 :: Int
  , value2 :: Int
  } deriving (Show)

getGear :: [EngineNumber] -> EngineIndicator -> Maybe Gear
getGear numbers EngineIndicator {..} =
  guard (symbol == '*' && length adjacentNumbers == 2)
    >> (Just
          $ Gear {value1 = head adjacentNumbers, value2 = adjacentNumbers !! 1})
  where
    adjacentNumbers = map value $ filter (location `isAdjacentTo`) numbers

ratio :: Gear -> Int
ratio Gear {..} = value1 * value2
