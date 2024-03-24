module Main where

import           Graph       (constructGraph)
import           Hike        (findLongestPath)
import           HikingMap   (parseHikingMap, parseNonSlipperyMap)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let hikingGraph = constructGraph $ parseHikingMap text
        nonSlipperyHikingGraph = constructGraph $ parseNonSlipperyMap text
    print $ findLongestPath hikingGraph
    print $ findLongestPath nonSlipperyHikingGraph
