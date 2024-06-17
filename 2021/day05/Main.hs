module Main where

import           Data.List   (group, sort)
import qualified Data.Text   as T
import           Line        (isAligned, parseLine, vents)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let ventLines = map parseLine $ T.lines text
    print $ countOverlap $ filter isAligned ventLines >>= vents
    print $ countOverlap $ ventLines >>= vents

countOverlap :: Ord a => [a] -> Int
countOverlap = length . filter ((> 1) . length) . group . sort
