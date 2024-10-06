module WaypointedShip where

import           Coordinate    (Coordinate (Coordinate), addCoordinate,
                                clockWise, counterClockWise, dilate, invert,
                                manhattanDistance)
import           Data.Function (on)
import           Data.List     (foldl')
import           Direction     (moveTowardsBy)
import           Instruction   (Instruction (..))

data WaypointedShip = WaypointedShip
  { ship     :: Coordinate
  , waypoint :: Coordinate
  } deriving (Show)

initialWaypointedShip :: WaypointedShip
initialWaypointedShip =
  WaypointedShip {ship = Coordinate 0 0, waypoint = Coordinate (-1) 10}

waypointedShipDistance :: WaypointedShip -> WaypointedShip -> Int
waypointedShipDistance = manhattanDistance `on` ship

setSailToWaypoint :: WaypointedShip -> [Instruction] -> WaypointedShip
setSailToWaypoint = foldl' processInstruction
  where
    processInstruction WaypointedShip {..} MoveTowards {..} =
      WaypointedShip
        {ship = ship, waypoint = (waypoint `moveTowardsBy` direction) amount}
    processInstruction WaypointedShip {..} MoveForward {..} =
      WaypointedShip
        { ship = ship `addCoordinate` (amount `dilate` waypoint)
        , waypoint = waypoint
        }
    processInstruction WaypointedShip {..} TurnClockWise =
      WaypointedShip {ship = ship, waypoint = clockWise waypoint}
    processInstruction WaypointedShip {..} TurnAround =
      WaypointedShip {ship = ship, waypoint = invert waypoint}
    processInstruction WaypointedShip {..} TurnCounterClockWise =
      WaypointedShip {ship = ship, waypoint = counterClockWise waypoint}
