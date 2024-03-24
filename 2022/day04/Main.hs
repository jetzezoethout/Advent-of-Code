module Main where

import qualified Data.Text   as T
import           Pair        (hasDoubleWork, hasRedundantCleaner, parsePair)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let pairs = map parsePair $ T.lines text
    print $ length $ filter hasRedundantCleaner pairs
    print $ length $ filter hasDoubleWork pairs
