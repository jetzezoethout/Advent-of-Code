module Main where

import           Cave          (parseCaveSystem)
import           LongPath      (longPathGenerator)
import           PathGenerator (allPaths)
import           ProcessFile   (processFile)
import           ShortPath     (shortPathGenerator)

main :: IO ()
main =
  processFile $ \text -> do
    let caveSystem = parseCaveSystem text
    print $ allPaths $ shortPathGenerator caveSystem
    print $ allPaths $ longPathGenerator caveSystem
