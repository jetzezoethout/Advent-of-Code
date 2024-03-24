module Box
  ( Box
  , emptyBox
  , remove
  , add
  , boxScore
  ) where

import           Data.Text (Text)

data LabeledLens = LabeledLens
  { label       :: Text
  , focalLength :: Int
  } deriving (Show)

newtype Box = Box
  { contents :: [LabeledLens]
  } deriving (Show)

emptyBox :: Box
emptyBox = Box []

remove :: Text -> Box -> Box
remove targetLabel Box {..} = Box $ go contents
  where
    go [] = []
    go (firstLens:remaining) =
      if label firstLens == targetLabel
        then remaining
        else firstLens : go remaining

add :: Text -> Int -> Box -> Box
add targetLabel newFocalLength Box {..} = Box $ go contents
  where
    lens = LabeledLens targetLabel newFocalLength
    go [] = [lens]
    go (firstLens:remaining) =
      if label firstLens == targetLabel
        then lens : remaining
        else firstLens : go remaining

boxScore :: Box -> Int
boxScore Box {..} = sum $ zipWith (*) [1 ..] $ map focalLength contents
