module Main where

import           Data.List   (sort)
import qualified Data.Text   as T
import           PlaneSeat   (parsePlaneSeat, seatId)
import           Prelude     hiding (Left, Right)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let planeSeats = map parsePlaneSeat $ T.lines text
        seatIds = map seatId planeSeats
    print $ maximum seatIds
    print $ gapIn seatIds

gapIn :: (Ord a, Enum a) => [a] -> a
gapIn = gapInSorted . sort
  where
    gapInSorted []     = error "cannot find gap in empty list"
    gapInSorted (x:xs) = go x xs
    go _ [] = error "no gap in this list"
    go previous (next:others) =
      if next == succ previous
        then go next others
        else succ previous
