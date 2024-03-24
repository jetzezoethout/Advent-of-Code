module Range where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Range = Range
  { start :: Int
  , end   :: Int
  } deriving (Show)

parseRange :: Text -> Range
parseRange text =
  let parts = T.split (== '-') text
   in Range (parseUnsignedInt $ head parts) (parseUnsignedInt $ parts !! 1)

isContainedIn :: Range -> Range -> Bool
r1 `isContainedIn` r2 = r1.start >= r2.start && r1.end <= r2.end

intersects :: Range -> Range -> Bool
r1 `intersects` r2 = r1.start <= r2.end && r2.start <= r1.end
