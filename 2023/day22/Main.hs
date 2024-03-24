module Main where

import           Brick        (parseBrick, sortByZ)
import qualified Data.Text    as T
import           FallenBricks (dropAllBricks)
import           ProcessFile  (processFile)
import           SettledBrick (yeetAllBricks)

main :: IO ()
main =
  processFile $ \text -> do
    let bricks = map parseBrick $ T.lines text
        sortedBricks = sortByZ bricks
        finalBricks = dropAllBricks sortedBricks
        yeetList = yeetAllBricks finalBricks
    print $ length $ filter (== 0) yeetList
    print $ sum yeetList
