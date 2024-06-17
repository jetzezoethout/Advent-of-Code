module CuboidCounter where

import           Cuboid     (Cuboid, cubes, intersect)
import           Data.Maybe (mapMaybe)

data CuboidCounter = CuboidCounter
  { positiveCuboids :: [Cuboid]
  , negativeCuboids :: [Cuboid]
  } deriving (Show)

emptyCounter :: CuboidCounter
emptyCounter = CuboidCounter [] []

totalCubes :: CuboidCounter -> Int
totalCubes CuboidCounter {..} =
  sum (map cubes positiveCuboids) - sum (map cubes negativeCuboids)

cut :: Cuboid -> CuboidCounter -> CuboidCounter
cut cuboid CuboidCounter {..} =
  CuboidCounter
    { positiveCuboids =
        mapMaybe (intersect cuboid) negativeCuboids <> positiveCuboids
    , negativeCuboids =
        mapMaybe (intersect cuboid) positiveCuboids <> negativeCuboids
    }

addPositive :: Cuboid -> CuboidCounter -> CuboidCounter
addPositive cuboid CuboidCounter {..} =
  CuboidCounter
    { positiveCuboids = cuboid : positiveCuboids
    , negativeCuboids = negativeCuboids
    }

turnOn :: Cuboid -> CuboidCounter -> CuboidCounter
turnOn cuboid = addPositive cuboid . cut cuboid

turnOff :: Cuboid -> CuboidCounter -> CuboidCounter
turnOff = cut
