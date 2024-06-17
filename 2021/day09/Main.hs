module Main where

import           Data.List   (sortBy)
import           Grid        (atCoordinate)
import           HeightMap   (basinSize, lowPoints, parseHeightMap)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let heightMap = parseHeightMap text
        lows = lowPoints heightMap
    print $ sum $ map ((+ 1) . (heightMap `atCoordinate`)) lows
    print
      $ product
      $ take 3
      $ sortBy (flip compare)
      $ map (basinSize heightMap) lows
