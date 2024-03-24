module Coordinate where

data Coordinate = Coordinate
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

det :: Coordinate -> Coordinate -> Int
det p1 p2 = p1.row * p2.column - p1.column * p2.row

manhattanDistance :: Coordinate -> Coordinate -> Int
manhattanDistance p1 p2 = abs (p1.row - p2.row) + abs (p1.column - p2.column)
