module Main where

import           City         (isFinalNode, parseCity)
import           OutgoingPath (findShortestPath, getPathsIn, getUltraPathsIn)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let city = parseCity text
    print $ findShortestPath (getPathsIn city) (isFinalNode city)
    print $ findShortestPath (getUltraPathsIn city) (isFinalNode city)
