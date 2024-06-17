module Line where

import           Coordinate (Coordinate (..))
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseUnsignedInt)

data Line = Line
  { start :: Coordinate
  , end   :: Coordinate
  }

parseLine :: Text -> Line
parseLine text =
  Line
    {start = parseCoordinate $ head parts, end = parseCoordinate $ parts !! 1}
  where
    parts = T.splitOn " -> " text
    parseCoordinate pair =
      let numbers = T.splitOn "," pair
       in Coordinate
            { row = parseUnsignedInt $ numbers !! 1
            , column = parseUnsignedInt $ head numbers
            }

isAligned :: Line -> Bool
isAligned Line {..} = start.row == end.row || start.column == end.column

vents :: Line -> [Coordinate]
vents Line {..} =
  [ Coordinate (start.row + drow * i) (start.column + dcol * i)
  | i <- [0 .. lineLength]
  ]
  where
    drow = signum $ end.row - start.row
    dcol = signum $ end.column - start.column
    lineLength =
      max (abs $ end.row - start.row) (abs $ end.column - start.column)
