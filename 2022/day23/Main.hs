module Main where

import           Grove       (emptySpaces, parseGrove, roundsNeeded,
                              spreadTimes)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let grove = parseGrove text
    print $ emptySpaces $ spreadTimes 10 grove
    print $ roundsNeeded grove
