module Calculations where

import           Coordinate (Coordinate (..))
import           RockGarden (RockGarden, reachableAfterSteps)

maxBlocks :: Int
maxBlocks = 26501365 `quot` 131

corners :: [Coordinate]
corners = [Coordinate i j | i <- [0, 130], j <- [0, 130]]

sideMidpoints :: [Coordinate]
sideMidpoints =
  [Coordinate i 65 | i <- [0, 130]] ++ [Coordinate 65 j | j <- [0, 130]]

centre :: Coordinate
centre = Coordinate 65 65

sidesContribution :: RockGarden -> Int
sidesContribution garden = sum $ map sideContribution corners
  where
    sideContribution corner =
      maxBlocks * reachableAfterSteps garden corner 64
        + (maxBlocks - 1) * reachableAfterSteps garden corner 195

cornersContribution :: RockGarden -> Int
cornersContribution garden = sum $ map cornerContribution sideMidpoints
  where
    cornerContribution sideMidpoint =
      reachableAfterSteps garden sideMidpoint 130

squared :: Int -> Int
squared x = x * x

insideContribution :: RockGarden -> Int
insideContribution garden =
  squared (maxBlocks - 1) * reachableAfterSteps garden centre 129
    + squared maxBlocks * reachableAfterSteps garden centre 130

getAnswer :: RockGarden -> Int
getAnswer garden =
  sidesContribution garden
    + cornersContribution garden
    + insideContribution garden
