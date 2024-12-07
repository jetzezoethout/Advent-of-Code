module EngineNumber where

import           Coordinate     (Coordinate (..))
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Data.Text.Read (decimal)

data TaggedRow = TaggedRow
  { rowIndex :: Int
  , content  :: Text
  } deriving (Show)

parseTaggedLines :: Text -> [TaggedRow]
parseTaggedLines = zipWith TaggedRow [0 ..] . T.lines

data EngineNumber = EngineNumber
  { leftMostLocation :: Coordinate
  , value            :: Int
  } deriving (Show)

parseEngineNumbers :: Text -> [EngineNumber]
parseEngineNumbers text = parseTaggedLines text >>= parseEngineNumbersOnRow

parseEngineNumbersOnRow :: TaggedRow -> [EngineNumber]
parseEngineNumbersOnRow TaggedRow {..} = getEngineNumbersStartingAt 0 content
  where
    getEngineNumbersStartingAt :: Int -> Text -> [EngineNumber]
    getEngineNumbersStartingAt _ "" = []
    getEngineNumbersStartingAt currentIndex text =
      case decimal text of
        Left _ -> getEngineNumbersStartingAt (currentIndex + 1) $ T.drop 1 text
        Right (value, remainder) -> location : otherLocations
          where valueLength = length $ show value
                location =
                  EngineNumber
                    { leftMostLocation =
                        Coordinate {row = rowIndex, column = currentIndex}
                    , value = value
                    }
                otherLocations =
                  getEngineNumbersStartingAt
                    (currentIndex + valueLength)
                    remainder

rightMostIndex :: EngineNumber -> Int
rightMostIndex EngineNumber {..} =
  leftMostLocation.column + length (show value) - 1

isAdjacentTo :: Coordinate -> EngineNumber -> Bool
Coordinate {..} `isAdjacentTo` engineNr@EngineNumber {..} =
  row >= leftMostLocation.row - 1
    && row <= leftMostLocation.row + 1
    && column >= leftMostLocation.column - 1
    && column <= rightMostIndex engineNr + 1
