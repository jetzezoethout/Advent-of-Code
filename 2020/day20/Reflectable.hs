module Reflectable where

import           Direction (Direction (..))
import           Rotatable (Rotatable (rotateClockWise), rotateCounterClockwise,
                            rotateFullTurn)

class Reflectable a where
  reflectRows :: a -> a

instance Reflectable Direction where
  reflectRows :: Direction -> Direction
  reflectRows North = South
  reflectRows South = North
  reflectRows West  = West
  reflectRows East  = East

reflectColumns :: (Rotatable a, Reflectable a) => a -> a
reflectColumns = rotateFullTurn . reflectRows

reflectDiagonal :: (Rotatable a, Reflectable a) => a -> a
reflectDiagonal = rotateClockWise . reflectRows

reflectAntiDiagonal :: (Rotatable a, Reflectable a) => a -> a
reflectAntiDiagonal = rotateCounterClockwise . reflectRows
