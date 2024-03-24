module Range where

import           Control.Monad (guard)
import           Coordinate    (Coordinate (..))
import           Data.List     (sort)
import           SensorData    (SensorData (..), radius)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show, Eq, Ord)

size :: Range -> Int
size Range {..} = end - start + 1

beaconlessShadow :: Int -> SensorData -> Maybe Range
beaconlessShadow rowIndex SensorData {..} =
  let rowDistance = abs $ sensor.row - rowIndex
      offset = radius - rowDistance
   in guard (offset >= 0)
        >> Just
             Range
               {start = sensor.column - offset, end = sensor.column + offset}

mergeRanges :: [Range] -> [Range]
mergeRanges ranges = go $ sort ranges
  where
    go (r1:r2:remaining) =
      if r1.end >= r2.start
        then go (Range {start = r1.start, end = max r1.end r2.end} : remaining)
        else r1 : go (r2 : remaining)
    go atMostOne = atMostOne
