module Main where

import           Burrow      (findOptimalSolution)
import           BurrowTypes (parseLargeBurrow, parseSmallBurrow)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let smallBurrow = parseSmallBurrow text
        largeBurrow = parseLargeBurrow text
    print $ findOptimalSolution smallBurrow
    print $ findOptimalSolution largeBurrow
