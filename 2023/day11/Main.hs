module Main where

import           ProcessFile (processFile)
import           Universe    (expand, parseUniverse, totalDistance)

main :: IO ()
main =
  processFile $ \text -> do
    let universe = parseUniverse text
        expandedUniverse = expand 2 universe
        superDuperUniverse = expand 1000000 universe
    print $ totalDistance expandedUniverse
    print $ totalDistance superDuperUniverse
