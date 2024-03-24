module Main where

import           ProcessFile (processFile)
import           Rope        (newRopeOfLength)
import           RopeJourney (parseDirections, tailLocations)

main :: IO ()
main =
  processFile $ \text -> do
    let journey = parseDirections text
        rope = newRopeOfLength 1
        longRope = newRopeOfLength 9
    print $ tailLocations rope journey
    print $ tailLocations longRope journey
