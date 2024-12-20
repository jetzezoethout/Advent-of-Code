module Universe where

import           Coordinate  (Coordinate (..), manhattanDistance)
import           Data.List   (nub, (\\))
import           Data.Text   (Text)
import           LocatedChar (LocatedChar (..), locateText)

newtype Universe = Universe
  { galaxies :: [Coordinate]
  } deriving (Show)

parseUniverse :: Text -> Universe
parseUniverse = Universe . map location . filter ((== '#') . char) . locateText

getEmptyRows :: Universe -> [Int]
getEmptyRows Universe {..} =
  let inhabitedRows = nub $ map row galaxies
      maxRow = maximum inhabitedRows
   in [0 .. maxRow] \\ inhabitedRows

getEmptyColumns :: Universe -> [Int]
getEmptyColumns Universe {..} =
  let inhabitedColumns = nub $ map column galaxies
      maxColumn = maximum inhabitedColumns
   in [0 .. maxColumn] \\ inhabitedColumns

getAmountSmaller :: Ord a => [a] -> a -> Int
getAmountSmaller sortedList element = length $ takeWhile (< element) sortedList

flyAway :: [Int] -> [Int] -> Int -> Coordinate -> Coordinate
flyAway bigRows bigColumns factor Coordinate {..} =
  let rowExpansion = getAmountSmaller bigRows row
      columnExpansion = getAmountSmaller bigColumns column
   in Coordinate
        { row = row + (factor - 1) * rowExpansion
        , column = column + (factor - 1) * columnExpansion
        }

expand :: Int -> Universe -> Universe
expand factor universe@Universe {..} =
  let bigRows = getEmptyRows universe
      bigColumns = getEmptyColumns universe
   in Universe $ map (flyAway bigRows bigColumns factor) galaxies

totalDistance :: Universe -> Int
totalDistance Universe {..} = go galaxies
  where
    go []     = 0
    go (p:ps) = sum (map (manhattanDistance p) ps) + go ps
