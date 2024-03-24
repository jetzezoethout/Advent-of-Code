module Polygon where

import           Coordinate        (Coordinate (..), det, manhattanDistance)
import           Direction         (moveTowardsBy)
import           TrenchInstruction (TrenchInstruction (..))

newtype Polygon = Polygon
  { vertices :: [Coordinate]
  } deriving (Show)

makePolygon :: [TrenchInstruction] -> Polygon
makePolygon instructions = Polygon $ go instructions $ Coordinate 0 0
  where
    go :: [TrenchInstruction] -> Coordinate -> [Coordinate]
    go [] _ = []
    go (TrenchInstruction {..}:remaining) coord =
      coord : go remaining (moveTowardsBy coord trenchDirection trenchLength)

boundaryPoints :: Polygon -> Int
boundaryPoints Polygon {..} =
  sum $ zipWith manhattanDistance vertices $ tail $ cycle vertices

doubleArea :: Polygon -> Int
doubleArea Polygon {..} =
  abs $ sum $ zipWith det vertices $ tail $ cycle vertices

interiorPoints :: Polygon -> Int
interiorPoints polygon =
  (doubleArea polygon - boundaryPoints polygon) `div` 2 + 1

totalPoints :: Polygon -> Int
totalPoints polygon = boundaryPoints polygon + interiorPoints polygon
