module RangeMappers where

import           Data.Maybe  (fromMaybe, mapMaybe)
import           Data.Monoid (First (..))
import           Data.Text   (Text)
import           Range       (Range, complement)
import           RangeMapper (RangeMapper (..), parseRangeMapper, shift,
                              singleImage)

type RangeMappers = [RangeMapper]

parseRangeMappers :: [Text] -> RangeMappers
parseRangeMappers dataLines = map parseRangeMapper $ tail dataLines

shiftAny :: Int -> RangeMappers -> Maybe Int
shiftAny value = getFirst . mconcat . map (First . shift value)

shiftWithFallThrough :: RangeMappers -> Int -> Int
shiftWithFallThrough mappers value = fromMaybe value $ shiftAny value mappers

processRangeMappers :: [RangeMappers] -> Int -> Int
processRangeMappers = foldr (flip (.) . shiftWithFallThrough) id

image :: RangeMappers -> [Range] -> [Range]
image [] ranges = ranges
image (mapper:mappers) ranges =
  mapMaybe (singleImage mapper) ranges <> image mappers unaffected
  where
    unaffected = ranges >>= (`complement` mapper.sourceRange)

processRangeMappersOnRanges :: [RangeMappers] -> [Range] -> [Range]
processRangeMappersOnRanges = foldr (flip (.) . image) id
