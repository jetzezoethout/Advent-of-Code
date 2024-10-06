module Main where

import qualified Data.Text   as T
import           NonLocal    (Dimensions (Dimensions, height, width),
                              nonLocalSeatRound)
import           ProcessFile (processFile)
import           Seat        (localSeatRound, occupiedSeats, parseWaitingArea)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        dimensions =
          Dimensions
            {height = length textLines, width = T.length $ head textLines}
        waitingArea = parseWaitingArea textLines
    print $ occupiedSeats $ stabilize localSeatRound waitingArea
    print $ occupiedSeats $ stabilize (nonLocalSeatRound dimensions) waitingArea

stabilize :: Eq a => (a -> a) -> a -> a
stabilize seatRound = go (0 :: Int)
  where
    go rounds waitingArea =
      let newWaitingArea = seatRound waitingArea
       in if waitingArea == newWaitingArea
            then waitingArea
            else go (rounds + 1) newWaitingArea
