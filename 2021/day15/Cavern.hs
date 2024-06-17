module Cavern where

import           Coordinate (Coordinate (..))
import           Data.Text  (Text)
import           Direction  (allDirections, moveTowards)
import           Grid       (Grid (..), atCoordinate, isInside, parseGrid)
import           PathFinder (SearchSpace (..))

type Cavern = Grid Int

parseCavern :: Text -> Cavern
parseCavern = parseGrid parseRiskLevel
  where
    parseRiskLevel ch = fromEnum ch - fromEnum '0'

small :: Grid Int -> SearchSpace
small cavern = SearchSpace {..}
  where
    neighbours :: Coordinate -> [Coordinate]
    neighbours coord =
      filter (`isInside` cavern) $ map (coord `moveTowards`) allDirections
    cost :: Coordinate -> Int
    cost coord = cavern `atCoordinate` coord
    isFinish :: Coordinate -> Bool
    isFinish Coordinate {..} =
      row == cavern.height - 1 && column == cavern.width - 1

wrapAroundNine :: Int -> Int
wrapAroundNine n = ((n - 1) `mod` 9) + 1

big :: Cavern -> SearchSpace
big cavern = SearchSpace {..}
  where
    neighbours :: Coordinate -> [Coordinate]
    neighbours coord = filter inside $ map (coord `moveTowards`) allDirections
      where
        inside Coordinate {..} =
          0 <= row
            && row < 5 * cavern.height
            && 0 <= column
            && column < 5 * cavern.width
    cost :: Coordinate -> Int
    cost Coordinate {..} =
      wrapAroundNine
        $ row `div` cavern.height
            + column `div` cavern.width
            + cavern
                `atCoordinate` Coordinate
                                 (row `mod` cavern.height)
                                 (column `mod` cavern.width)
    isFinish :: Coordinate -> Bool
    isFinish Coordinate {..} =
      row == 5 * cavern.height - 1 && column == 5 * cavern.width - 1
