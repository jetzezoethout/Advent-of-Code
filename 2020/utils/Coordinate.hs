module Coordinate where

data Coordinate = Coordinate
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

addCoordinate :: Coordinate -> Coordinate -> Coordinate
c1 `addCoordinate` c2 =
  Coordinate {row = c1.row + c2.row, column = c1.column + c2.column}

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance c1 c2 = abs (c1.row - c2.row) + abs (c1.column - c2.column)

clockWise :: Coordinate -> Coordinate
clockWise Coordinate {..} = Coordinate {row = column, column = -row}

invert :: Coordinate -> Coordinate
invert Coordinate {..} = Coordinate {row = -row, column = -column}

counterClockWise :: Coordinate -> Coordinate
counterClockWise Coordinate {..} = Coordinate {row = -column, column = row}

dilate :: Int -> Coordinate -> Coordinate
factor `dilate` Coordinate {..} =
  Coordinate {row = factor * row, column = factor * column}
