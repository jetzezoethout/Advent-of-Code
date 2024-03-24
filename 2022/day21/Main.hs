module Main where

import           MonkeyTree  (adjustRoot, evaluate, findHumanValue,
                              parseMonkeyTree)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let monkeyTree = parseMonkeyTree text
    print $ evaluate monkeyTree
    print $ findHumanValue $ adjustRoot monkeyTree
