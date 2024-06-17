module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           RebootStep  (applyRebootSteps, parseRebootStep)

main :: IO ()
main =
  processFile $ \text -> do
    let rebootSteps = map parseRebootStep $ T.lines text
    print $ applyRebootSteps $ take 20 rebootSteps
    print $ applyRebootSteps rebootSteps
