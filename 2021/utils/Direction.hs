module Direction where

import           Coordinate (Coordinate (..))

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Ord, Show, Enum)

allDirections :: [Direction]
allDirections = [North .. West]

moveTowards :: Coordinate -> Direction -> Coordinate
moveTowards Coordinate {..} dir =
  case dir of
    North -> Coordinate {row = row - 1, column = column}
    East  -> Coordinate {row = row, column = column + 1}
    South -> Coordinate {row = row + 1, column = column}
    West  -> Coordinate {row = row, column = column - 1}

moveTowardsBy :: Coordinate -> Direction -> Int -> Coordinate
moveTowardsBy Coordinate {..} dir distance =
  case dir of
    North -> Coordinate {row = row - distance, column = column}
    East  -> Coordinate {row = row, column = column + distance}
    South -> Coordinate {row = row + distance, column = column}
    West  -> Coordinate {row = row, column = column - distance}
