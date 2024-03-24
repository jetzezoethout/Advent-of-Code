module Rope where

import           Coordinate (Coordinate (..))
import           Direction  (Direction, moveTowards)

data Rope = Rope
  { ropeHead  :: Coordinate
  , ropeTails :: [Coordinate]
  }

ropeTail :: Rope -> Coordinate
ropeTail Rope {..} = last ropeTails

newRopeOfLength :: Int -> Rope
newRopeOfLength n = Rope (Coordinate 0 0) $ replicate n $ Coordinate 0 0

isAdjacentTo :: Coordinate -> Coordinate -> Bool
c1 `isAdjacentTo` c2 =
  abs (c1.column - c2.column) <= 1 && abs (c1.row - c2.row) <= 1

pullTails :: Coordinate -> [Coordinate] -> [Coordinate]
pullTails _ [] = []
pullTails headPosition (tailPosition:otherTails) =
  let newTailPosition = pullTail headPosition tailPosition
   in newTailPosition : pullTails newTailPosition otherTails

pullTail :: Coordinate -> Coordinate -> Coordinate
pullTail headPosition tailPosition =
  if headPosition `isAdjacentTo` tailPosition
    then tailPosition
    else Coordinate
           { row = pullTailOneDimensional headPosition.row tailPosition.row
           , column =
               pullTailOneDimensional headPosition.column tailPosition.column
           }

pullTailOneDimensional :: Int -> Int -> Int
pullTailOneDimensional headPosition tailPosition
  | headPosition > tailPosition = tailPosition + 1
  | headPosition < tailPosition = tailPosition - 1
  | otherwise = tailPosition

moveRopeTowards :: Rope -> Direction -> Rope
Rope {..} `moveRopeTowards` direction =
  Rope {ropeHead = newHead, ropeTails = pullTails newHead ropeTails}
  where
    newHead = ropeHead `moveTowards` direction
