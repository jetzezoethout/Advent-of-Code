module RangeMapper where

import           Control.Monad (guard)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseUnsignedInt)
import           Range         (Range (..), contains, intersect, shiftRange)

data RangeMapper = RangeMapper
  { destinationStart :: Int
  , sourceRange      :: Range
  } deriving (Show)

parseRangeMapper :: Text -> RangeMapper
parseRangeMapper text =
  RangeMapper
    { destinationStart = head numbers
    , sourceRange =
        Range {start = numbers !! 1, end = numbers !! 1 + theLength - 1}
    }
  where
    numbers = map parseUnsignedInt $ T.words text
    theLength = numbers !! 2

shift :: Int -> RangeMapper -> Maybe Int
shift value RangeMapper {..} =
  guard (sourceRange `contains` value)
    >> Just (value - sourceRange.start + destinationStart)

singleImage :: RangeMapper -> Range -> Maybe Range
singleImage RangeMapper {..} range =
  shiftRange (destinationStart - sourceRange.start)
    <$> range `intersect` sourceRange
