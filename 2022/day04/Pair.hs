module Pair where

import           Data.Text (Text)
import qualified Data.Text as T
import           Range     (Range, intersects, isContainedIn, parseRange)

data Pair = Pair
  { firstRange  :: Range
  , secondRange :: Range
  } deriving (Show)

parsePair :: Text -> Pair
parsePair text =
  let parts = T.split (== ',') text
   in Pair (parseRange $ head parts) (parseRange $ parts !! 1)

hasRedundantCleaner :: Pair -> Bool
hasRedundantCleaner Pair {..} =
  firstRange `isContainedIn` secondRange
    || secondRange `isContainedIn` firstRange

hasDoubleWork :: Pair -> Bool
hasDoubleWork Pair {..} = firstRange `intersects` secondRange
