module Main where

import           Droplet     (exteriorSurfaceArea, parseDroplet, surfaceArea)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let droplet = parseDroplet text
    print $ surfaceArea droplet
    print $ exteriorSurfaceArea droplet
