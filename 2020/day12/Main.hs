module Main where

import qualified Data.Text      as T
import           Instruction    (parseInstruction)
import           ProcessFile    (processFile)
import           Ship           (initialShip, setSail, shipDistance)
import           WaypointedShip (initialWaypointedShip, setSailToWaypoint,
                                 waypointedShipDistance)

main :: IO ()
main =
  processFile $ \text -> do
    let instructions = map parseInstruction $ T.lines text
    print $ shipDistance initialShip $ setSail initialShip instructions
    print
      $ waypointedShipDistance initialWaypointedShip
      $ setSailToWaypoint initialWaypointedShip instructions
