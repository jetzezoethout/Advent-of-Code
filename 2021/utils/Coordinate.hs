module Coordinate where

data Coordinate = Coordinate
  { row    :: Int
  , column :: Int
  } deriving (Eq, Ord, Show)

addCoordinate :: Coordinate -> Coordinate -> Coordinate
c1 `addCoordinate` c2 =
  Coordinate {row = c1.row + c2.row, column = c1.column + c2.column}
