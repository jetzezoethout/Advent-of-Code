module Rock where

import           Coordinate (Coordinate (..))
import           Direction

data Shape
  = HorizontalBar
  | Plus
  | Hook
  | VerticalBar
  | Square
  deriving (Show, Eq, Enum, Bounded)

allShapes :: [Shape]
allShapes = [minBound .. maxBound]

newtype Rock = Rock
  { getRocks :: [Coordinate]
  } deriving (Show)

makeRock :: Coordinate -> Shape -> Rock
makeRock Coordinate {..} HorizontalBar =
  Rock [Coordinate row (column + dc) | dc <- [0 .. 3]]
makeRock Coordinate {..} Plus =
  Rock
    $ [ Coordinate (row + dr) (column + dc)
      | (dr, dc) <- [(-2, 1), (-1, 0), (-1, 1), (-1, 2), (0, 1)]
      ]
makeRock Coordinate {..} Hook =
  Rock
    $ [ Coordinate (row + dr) (column + dc)
      | (dr, dc) <- [(-2, 2), (-1, 2), (0, 0), (0, 1), (0, 2)]
      ]
makeRock Coordinate {..} VerticalBar =
  Rock $ [Coordinate (row + dr) column | dr <- [-3 .. 0]]
makeRock Coordinate {..} Square =
  Rock $ [Coordinate (row + dr) (column + dc) | dr <- [-1, 0], dc <- [0, 1]]

moveRockTowards :: Rock -> Direction -> Rock
Rock {..} `moveRockTowards` direction =
  Rock $ map (`moveTowards` direction) getRocks

hasPartWith :: Rock -> (Coordinate -> Bool) -> Bool
Rock {..} `hasPartWith` p = any p getRocks
