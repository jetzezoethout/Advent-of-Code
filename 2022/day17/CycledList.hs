module CycledList where

import           Cave                (emptyCave, getHeights)
import           Control.Monad.State (evalState)
import           Jet                 (Jet)
import           Rock                (allShapes)

data CycledList = CycledList
  { initialSegment :: [Int]
  , cycledSegment  :: [Int]
  } deriving (Show)

atIndex :: CycledList -> Int -> Int
CycledList {..} `atIndex` i =
  let initialLength = length initialSegment
      cycleLength = length cycledSegment
      cycleIncrement = last cycledSegment - last initialSegment
   in if i < initialLength
        then initialSegment !! i
        else cycledSegment !! ((i - initialLength) `mod` cycleLength)
               + cycleIncrement * ((i - initialLength) `div` cycleLength)

heightsCycle :: [Jet] -> CycledList
heightsCycle jets = evalState go emptyCave
  where
    go = do
      initial <- getHeights (cycle allShapes) jets
      let shapesOffset = length initial `mod` length allShapes
      cycled <- getHeights (drop shapesOffset $ cycle allShapes) jets
      return $ CycledList initial cycled
