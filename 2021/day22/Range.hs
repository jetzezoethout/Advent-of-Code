module Range where

import           Control.Monad (guard)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseInt)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show)

parseRange :: Text -> Range
parseRange text =
  let parts = T.splitOn ".." text
   in Range {start = parseInt $ head parts, end = parseInt $ parts !! 1}

makeRange :: Int -> Int -> Maybe Range
makeRange start end = guard (start <= end) >> Just (Range start end)

intersectRanges :: Range -> Range -> Maybe Range
intersectRanges r1 r2 = makeRange (max r1.start r2.start) (min r1.end r2.end)

size :: Range -> Int
size Range {..} = end - start + 1
