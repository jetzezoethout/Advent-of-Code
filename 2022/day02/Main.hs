module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           Round       (parseFromGoal, parseFromMoves, roundScore)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        wrongRounds = map parseFromMoves textLines
        correctRounds = map parseFromGoal textLines
    print $ sum $ map roundScore wrongRounds
    print $ sum $ map roundScore correctRounds
