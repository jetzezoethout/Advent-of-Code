module Main where

import           City          (parseCity)
import           Data.Maybe    (fromJust)
import           OutgoingPath  (getPathsFrom, getUltraPathsFrom)
import           ProcessFile   (processFile)
import           SearchContext (findShortestPath, runSearchContext)

main :: IO ()
main =
  processFile $ \text -> do
    let city = parseCity text
    print $ fromJust $ runSearchContext city $ findShortestPath getPathsFrom
    print
      $ fromJust
      $ runSearchContext city
      $ findShortestPath getUltraPathsFrom
