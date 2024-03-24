module Main where

import           Cave        (parseCave)
import           LightBeam   (getEnergizedTiles, getMaxEnergizable, initialBeam)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let cave = parseCave text
    print $ getEnergizedTiles initialBeam cave
    print $ getMaxEnergizable cave
