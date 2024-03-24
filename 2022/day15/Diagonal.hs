module Diagonal where

import           Coordinate (Coordinate (..))
import           SensorData (SensorData (..))

newtype Diagonal = Diagonal
  { rowMinusColumn :: Int
  } deriving (Show, Eq)

borderNWDiagonal :: SensorData -> Diagonal
borderNWDiagonal SensorData {..} =
  Diagonal $ sensor.row - sensor.column - (radius + 1)

borderSEDiagonal :: SensorData -> Diagonal
borderSEDiagonal SensorData {..} =
  Diagonal $ sensor.row - sensor.column + (radius + 1)

newtype AntiDiagonal = AntiDiagonal
  { rowPlusColumn :: Int
  } deriving (Show, Eq)

borderSWAntidiagonal :: SensorData -> AntiDiagonal
borderSWAntidiagonal SensorData {..} =
  AntiDiagonal $ sensor.row + sensor.column - (radius + 1)

borderNEAntidiagonal :: SensorData -> AntiDiagonal
borderNEAntidiagonal SensorData {..} =
  AntiDiagonal $ sensor.row + sensor.column + (radius + 1)

intersectDiagonals :: Diagonal -> AntiDiagonal -> Coordinate
intersectDiagonals Diagonal {..} AntiDiagonal {..} =
  Coordinate
    { row = (rowPlusColumn + rowMinusColumn) `div` 2
    , column = (rowPlusColumn - rowMinusColumn) `div` 2
    }
