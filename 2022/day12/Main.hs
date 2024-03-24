module Main where

import           HeightMap   (parseHeightMap)
import           Hike        (descend)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let heightMap = parseHeightMap text
    print $ descend heightMap (== 0)
    print $ descend heightMap (== 1)
