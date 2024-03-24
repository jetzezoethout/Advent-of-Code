module CycledList where

data CycledList a = CycledList
  { initialSegment :: [a]
  , cycledSegment  :: [a]
  } deriving (Show)

atIndex :: CycledList a -> Int -> a
CycledList {..} `atIndex` n =
  let initialSize = length initialSegment
      cycleSize = length cycledSegment
   in if n < initialSize
        then initialSegment !! n
        else cycledSegment !! ((n - initialSize) `mod` cycleSize)
