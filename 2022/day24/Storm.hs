module Storm where

import           Blizzard    (atTime, parseBlizzards)
import           Coordinate  (Coordinate (..))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, generate, (!))

data Storm = Storm
  { height            :: Int
  , width             :: Int
  , cycleLength       :: Int
  , blockedTimeSeries :: Vector (Set Coordinate)
  } deriving (Show)

start :: Storm -> Coordinate
start _ = Coordinate (-1) 0

finish :: Storm -> Coordinate
finish Storm {..} = Coordinate height (width - 1)

shiftInside :: Int -> Int -> Coordinate -> Coordinate
shiftInside height width Coordinate {..} =
  Coordinate (row `mod` height) (column `mod` width)

parseStorm :: [Text] -> Storm
parseStorm textLines =
  Storm
    { height = height
    , width = width
    , cycleLength = cycleLength
    , blockedTimeSeries = generate cycleLength blockedAtTime
    }
  where
    height = length textLines
    width = T.length $ head textLines
    cycleLength = lcm height width
    blizzards = parseBlizzards textLines
    blockedAtTime t =
      S.fromList $ map (shiftInside height width . (`atTime` t)) blizzards

isFreeAtStage :: Storm -> Int -> Coordinate -> Bool
isFreeAtStage storm t coord =
  isAccessible storm coord && coord `S.notMember` (storm.blockedTimeSeries ! t)

isAccessible :: Storm -> Coordinate -> Bool
isAccessible storm coord =
  coord == start storm
    || coord == finish storm
    || (0 <= coord.row
          && coord.row < storm.height
          && 0 <= coord.column
          && coord.column < storm.width)
