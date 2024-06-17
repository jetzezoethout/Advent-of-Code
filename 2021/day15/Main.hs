module Main where

import           Cavern      (big, parseCavern, small)
import           PathFinder  (findShortestPath)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let cavern = parseCavern text
    print $ findShortestPath $ small cavern
    print $ findShortestPath $ big cavern
