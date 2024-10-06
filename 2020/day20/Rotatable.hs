module Rotatable where

import           Direction (Direction, clockWise)

class Rotatable a where
  rotateClockWise :: a -> a

instance Rotatable Direction where
  rotateClockWise :: Direction -> Direction
  rotateClockWise = clockWise

rotateFullTurn :: Rotatable a => a -> a
rotateFullTurn = rotateClockWise . rotateClockWise

rotateCounterClockwise :: Rotatable a => a -> a
rotateCounterClockwise = rotateClockWise . rotateFullTurn

rotateUntil :: Rotatable a => (a -> Bool) -> a -> a
rotateUntil p x =
  if p x
    then x
    else rotateUntil p $ rotateClockWise x
