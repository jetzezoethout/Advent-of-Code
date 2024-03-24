module Main where

import qualified Data.Text    as T
import           ProcessFile  (processFile)
import           Storm        (finish, parseStorm, start)
import           StormJourney (shortestPath)

main :: IO ()
main =
  processFile $ \text -> do
    let relevantLines = map (T.tail . T.init) $ tail $ init $ T.lines text
        storm = parseStorm relevantLines
        startPosition = start storm
        finishPosition = finish storm
        firstLeg = shortestPath storm 0 startPosition finishPosition
        secondLeg = shortestPath storm firstLeg finishPosition startPosition
        thirdLeg =
          shortestPath storm (firstLeg + secondLeg) startPosition finishPosition
    print firstLeg
    print $ firstLeg + secondLeg + thirdLeg
