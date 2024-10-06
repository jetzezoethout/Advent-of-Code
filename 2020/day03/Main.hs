module Main where

import           Forest      (encounterTrees, parseForest)
import           ProcessFile (processFile)
import           Slope       (Slope (Slope))

main :: IO ()
main =
  processFile $ \text -> do
    let forest = parseForest text
    print $ encounterTrees forest $ Slope 3 1
    print
      $ product
      $ map
          (encounterTrees forest)
          [Slope 1 1, Slope 3 1, Slope 5 1, Slope 7 1, Slope 1 2]
