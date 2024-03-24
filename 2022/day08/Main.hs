module Main where

import           Coordinate  (Coordinate (Coordinate))
import           Forest      (isVisibleFromOutside, parseForest, scenicScore)
import           Grid        (Grid (..))
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let forest = parseForest text
        allLocations =
          [ Coordinate i j
          | i <- [0 .. height forest - 1]
          , j <- [0 .. width forest - 1]
          ]
    print $ length $ filter (isVisibleFromOutside forest) allLocations
    print $ maximum $ map (scenicScore forest) allLocations
