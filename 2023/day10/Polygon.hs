module Polygon where

import           Coordinate (Coordinate, det)
import           Direction  (moveTowards)
import           Grid       (atCoordinate)
import           Pipe       (Pipe (..), unsafeContinue)
import           PipeMap    (PipeMap, findSomewhereToGo, findStart)

newtype Polygon = Polygon
  { boundary :: [Coordinate]
  }

boundaryPoints :: Polygon -> Int
boundaryPoints Polygon {..} = length boundary

doubleArea :: Polygon -> Int
doubleArea Polygon {..} =
  abs $ sum $ zipWith det boundary $ tail $ cycle boundary

interiorPoints :: Polygon -> Int
interiorPoints polygon =
  (doubleArea polygon - boundaryPoints polygon) `div` 2 + 1

traverseBoundary :: PipeMap -> Polygon
traverseBoundary pipeMap = Polygon $ go startingPoint startingDirection
  where
    startingPoint = findStart pipeMap
    startingDirection = findSomewhereToGo pipeMap startingPoint
    go currentCoordinate nextDirection =
      let nextCoordinate = currentCoordinate `moveTowards` nextDirection
          nextPipe = pipeMap `atCoordinate` nextCoordinate
       in currentCoordinate
            : case nextPipe of
                Start -> []
                _ -> go nextCoordinate $ unsafeContinue nextDirection nextPipe
