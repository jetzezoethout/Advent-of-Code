module Main
  ( main
  ) where

import           Boxes       (processAll, score)
import qualified Data.Text   as T
import           Instruction (hash, parseInstruction)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let itemsToHash = T.split (== ',') text
        instructions = map parseInstruction itemsToHash
        finalBoxState = processAll instructions
    print $ sum $ map hash itemsToHash
    print $ score finalBoxState
