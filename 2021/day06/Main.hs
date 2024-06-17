module Main where

import           ProcessFile (processFile)
import           School      (evolveTimes, parseSchool, totalFish)

main :: IO ()
main =
  processFile $ \text -> do
    let school = parseSchool text
    print $ totalFish $ evolveTimes 80 school
    print $ totalFish $ evolveTimes 256 school
