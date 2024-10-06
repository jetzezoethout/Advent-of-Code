module Ship where

import           Coordinate    (Coordinate (Coordinate), manhattanDistance)
import           Data.Function (on)
import           Data.List     (foldl')
import           Direction     (Direction (East), clockWise, counterClockWise,
                                invert, moveTowardsBy)
import           Instruction   (Instruction (..))

data Ship = Ship
  { location :: Coordinate
  , facing   :: Direction
  } deriving (Show)

initialShip :: Ship
initialShip = Ship {location = Coordinate 0 0, facing = East}

shipDistance :: Ship -> Ship -> Int
shipDistance = manhattanDistance `on` location

setSail :: Ship -> [Instruction] -> Ship
setSail = foldl' processInstruction
  where
    processInstruction :: Ship -> Instruction -> Ship
    processInstruction Ship {..} MoveTowards {..} =
      Ship
        { location = (location `moveTowardsBy` direction) amount
        , facing = facing
        }
    processInstruction Ship {..} MoveForward {..} =
      Ship
        {location = (location `moveTowardsBy` facing) amount, facing = facing}
    processInstruction Ship {..} TurnClockWise =
      Ship {location = location, facing = clockWise facing}
    processInstruction Ship {..} TurnAround =
      Ship {location = location, facing = invert facing}
    processInstruction Ship {..} TurnCounterClockWise =
      Ship {location = location, facing = counterClockWise facing}
