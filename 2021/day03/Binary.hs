module Binary where

import           Bit           (Bit (..), bitToInt, fromChar, invertBit)
import           Data.Foldable (foldl')
import           Data.Maybe    (fromJust, fromMaybe)
import           Data.Text     (Text)
import qualified Data.Text     as T

newtype Binary = Binary
  { getBits :: [Bit]
  } deriving (Show)

parseBinary :: Text -> Binary
parseBinary = Binary . map fromChar . T.unpack

invert :: Binary -> Binary
invert = Binary . map invertBit . getBits

toInt :: Binary -> Int
toInt = foldl' (\acc bit -> 2 * acc + bitToInt bit) 0 . getBits

atPosition :: Binary -> Int -> Bit
Binary {..} `atPosition` i = getBits !! i

size :: Binary -> Int
size = length . getBits

mostCommonBit :: [Binary] -> Int -> Maybe Bit
mostCommonBit report i = go 0 0 report
  where
    go :: Int -> Int -> [Binary] -> Maybe Bit
    go zeroes ones [] =
      case zeroes `compare` ones of
        LT -> Just One
        EQ -> Nothing
        GT -> Just Zero
    go zeroes ones (next:others) =
      case next `atPosition` i of
        Zero -> go (zeroes + 1) ones others
        One  -> go zeroes (ones + 1) others

getGammaRate :: [Binary] -> Binary
getGammaRate report =
  Binary $ map (fromJust . mostCommonBit report) [0 .. size (head report) - 1]

getOxygenRating :: [Binary] -> Binary
getOxygenRating = go 0
  where
    go _ [] = error "this should not happen"
    go _ [candidate] = candidate
    go i candidates =
      let target = fromMaybe One $ mostCommonBit candidates i
       in go (i + 1) $ filter ((== target) . (`atPosition` i)) candidates

getCO2Rating :: [Binary] -> Binary
getCO2Rating = go 0
  where
    go _ [] = error "this should not happen"
    go _ [candidate] = candidate
    go i candidates =
      let target = maybe Zero invertBit $ mostCommonBit candidates i
       in go (i + 1) $ filter ((== target) . (`atPosition` i)) candidates
