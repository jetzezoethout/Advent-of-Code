module Range where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show)

intersect :: Range -> Range -> Range
range1 `intersect` range2 =
  Range (max range1.start range2.start) (min range1.end range2.end)

maxRange :: Range
maxRange = Range 1 4000

parseRange :: Text -> Range
parseRange text =
  case T.head text of
    '<' -> Range 1 (threshold - 1)
    '>' -> Range (threshold + 1) 4000
    _   -> error "invalid range"
  where
    threshold = parseUnsignedInt $ T.tail text

liesInside :: Int -> Range -> Bool
number `liesInside` Range {..} = start <= number && number <= end

firstComplement :: Range -> Range
firstComplement Range {..} =
  if start == 1
    then Range (end + 1) 4000
    else Range 1 (start - 1)

numberOfElements :: Range -> Int
numberOfElements Range {..} = max 0 $ end - start + 1
