module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           Race        (Race, numberOfWinTimes, parseRaces,
                              parseSingleRace)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        timeLine = head textLines
        distanceLine = textLines !! 1
        races = parseRaces timeLine distanceLine
        singleRace = parseSingleRace timeLine distanceLine
    print $ getAnswer1 races
    print $ numberOfWinTimes singleRace

getAnswer1 :: [Race] -> Int
getAnswer1 = product . map numberOfWinTimes
