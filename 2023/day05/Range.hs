module Range where

import           Control.Monad   (guard)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (catMaybes)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseUnsignedInt)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show)

makeRange :: Int -> Int -> Maybe Range
makeRange start end = guard (start <= end) >> Just (Range start end)

parseRanges :: Text -> [Range]
parseRanges text =
  map fromPair . chunksOf 2
    $ map parseUnsignedInt
    $ T.words
    $ T.split (== ':') text !! 1
  where
    fromPair :: [Int] -> Range
    fromPair numbers =
      Range {start = head numbers, end = head numbers + numbers !! 1 - 1}

intersect :: Range -> Range -> Maybe Range
range1 `intersect` range2 =
  makeRange (max range1.start range2.start) (min range1.end range2.end)

complement :: Range -> Range -> [Range]
range1 `complement` range2 =
  let leftPart = makeRange range1.start (min range1.end (range2.start - 1))
      rightPart = makeRange (max range1.start (range2.end + 1)) range1.end
   in catMaybes [leftPart, rightPart]

contains :: Range -> Int -> Bool
Range {..} `contains` value = start <= value && value <= end

shiftRange :: Int -> Range -> Range
shiftRange shift Range {..} = Range {start = start + shift, end = end + shift}

lowest :: [Range] -> Int
lowest = minimum . map start
