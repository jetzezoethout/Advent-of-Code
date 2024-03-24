module Main where

import           Data.Ratio  (numerator, (%))
import qualified Data.Text   as T
import           HailStone   (allIntersections, findRock, flatten,
                              parseHailStone)
import           ProcessFile (processFile)
import           Vector      (checkBounds)

main :: IO ()
main =
  processFile $ \text -> do
    let hailStones = map parseHailStone $ T.lines text
        flatHailStones = map flatten hailStones
        intersections = allIntersections flatHailStones
        intersectionsInsideArea =
          filter
            (checkBounds (200000000000000 % 1) (400000000000000 % 1))
            intersections
    print $ length intersectionsInsideArea
    print $ numerator $ sum $ findRock hailStones
