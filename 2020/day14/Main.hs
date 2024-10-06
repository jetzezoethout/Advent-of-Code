module Main where

import           DecoderChip (execute, executeV2, newMachine, totalMemory)
import           ProcessFile (processFile)
import           Program     (parseProgram)

main :: IO ()
main =
  processFile $ \text -> do
    let program = parseProgram text
    print $ totalMemory $ execute newMachine program
    print $ totalMemory $ executeV2 newMachine program
