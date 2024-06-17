module Main where

import           PhaseSpace  (initialVelocities)
import           ProcessFile (processFile)
import           TargetArea  (highestY, parseTargetArea)

main :: IO ()
main =
  processFile $ \text -> do
    let targetArea = parseTargetArea text
    print $ highestY targetArea
    print $ initialVelocities targetArea
