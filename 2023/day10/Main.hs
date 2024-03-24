module Main where

import           PipeMap     (parsePipeMap)
import           Polygon     (boundaryPoints, interiorPoints, traverseBoundary)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let pipeMap = parsePipeMap text
        polygon = traverseBoundary pipeMap
    print $ boundaryPoints polygon `div` 2
    print $ interiorPoints polygon
