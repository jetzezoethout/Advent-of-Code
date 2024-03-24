module Forest where

import           Coordinate (Coordinate)
import           Data.Char  (digitToInt)
import           Data.Text  (Text)
import           Direction  (Direction, allDirections, moveTowards)
import           Grid       (Grid, atCoordinate, parseGrid, safeAtCoordinate)

type Forest = Grid Int

parseForest :: Text -> Grid Int
parseForest = parseGrid digitToInt

isVisibleFromOutside :: Forest -> Coordinate -> Bool
isVisibleFromOutside forest location =
  any (canSeeOutsideTowards forest location) allDirections

canSeeOutsideTowards :: Forest -> Coordinate -> Direction -> Bool
canSeeOutsideTowards forest location direction = go location
  where
    ourTree = forest `atCoordinate` location
    go currentLocation =
      let nextLocation = currentLocation `moveTowards` direction
       in case forest `safeAtCoordinate` nextLocation of
            Just tree -> (tree < ourTree) && go nextLocation
            Nothing   -> True

scenicScore :: Forest -> Coordinate -> Int
scenicScore forest location =
  product $ map (viewTowards forest location) allDirections

viewTowards :: Forest -> Coordinate -> Direction -> Int
viewTowards forest location direction = go location 0
  where
    ourTree = forest `atCoordinate` location
    go currentLocation seenSoFar =
      let nextLocation = currentLocation `moveTowards` direction
       in case forest `safeAtCoordinate` nextLocation of
            Just tree ->
              if tree < ourTree
                then go nextLocation (seenSoFar + 1)
                else seenSoFar + 1
            Nothing -> seenSoFar
