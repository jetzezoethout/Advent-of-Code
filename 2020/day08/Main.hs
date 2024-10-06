module Main where

import           ProcessFile (processFile)
import           Program     (parseProgram, repairProgram, runUntilInfiniteLoop)

main :: IO ()
main =
  processFile $ \text -> do
    let program = parseProgram text
    print $ runUntilInfiniteLoop program
    print $ repairProgram program
