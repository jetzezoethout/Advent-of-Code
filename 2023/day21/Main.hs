module Main where

import           Calculations (getAnswer)
import           Coordinate   (Coordinate (..))
import           ProcessFile  (processFile)
import           RockGarden   (parseRockGarden, reachableAfterSteps)

main :: IO ()
main =
  processFile $ \text -> do
    let rockGarden = parseRockGarden text
    print $ reachableAfterSteps rockGarden (Coordinate 65 65) 64
    print $ getAnswer rockGarden
