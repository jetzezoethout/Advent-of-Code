module Main where

import qualified Part1       as P1
import qualified Part2       as P2
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    print $ P1.getAnswer text
    print $ P2.getAnswer text
