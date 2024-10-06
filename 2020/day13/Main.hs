module Main where

import           Bus           (Bus (busId), nextDepartureFrom, parseBusses)
import           CRTSolver     (perfectDepartureTime)
import           Data.Function (on)
import           Data.List     (minimumBy)
import qualified Data.Text     as T
import           Parsers       (parseUnsignedInt)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        now = parseUnsignedInt $ head textLines
        busses = parseBusses $ textLines !! 1
        (firstBus, waitingTime) =
          minimumBy (compare `on` snd)
            $ zip busses
            $ map (nextDepartureFrom now) busses
    print $ firstBus.busId * waitingTime
    print $ perfectDepartureTime busses
