module Coordinate where

data Coordinate = Coordinate
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance p1 p2 = abs (p1.row - p2.row) + abs (p1.column - p2.column)

addCoordinates :: Coordinate -> Coordinate -> Coordinate
c1 `addCoordinates` c2 = Coordinate (c1.row + c2.row) (c1.column + c2.column)
