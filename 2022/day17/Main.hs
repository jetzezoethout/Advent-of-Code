module Main where

import           CycledList  (atIndex, heightsCycle)
import           Jet         (parseJets)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let jets = parseJets text
        heights = heightsCycle jets
    print $ heights `atIndex` (2022 - 1)
    print $ heights `atIndex` (1000000000000 - 1)
