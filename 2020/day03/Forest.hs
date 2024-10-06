module Forest where

import           Coordinate (Coordinate (..))
import           Data.Text  (Text)
import           Grid       (Grid (..), atCoordinate, parseGrid)
import           Slope      (Slope, atTime)

newtype Forest =
  Forest (Grid Bool)

parseForest :: Text -> Forest
parseForest = Forest . parseGrid (== '#')

hasTreeAt :: Forest -> Coordinate -> Bool
(Forest forest) `hasTreeAt` coord = forest `atCoordinate` shiftOntoMap coord
  where
    shiftOntoMap Coordinate {..} =
      Coordinate {row = row, column = column `mod` forest.width}

isInside :: Coordinate -> Forest -> Bool
Coordinate {..} `isInside` (Forest forest) = 0 <= row && row < forest.height

encounterTrees :: Forest -> Slope -> Int
encounterTrees forest slope =
  length
    $ filter (forest `hasTreeAt`)
    $ takeWhile (`isInside` forest)
    $ map (slope `atTime`) [0 ..]
