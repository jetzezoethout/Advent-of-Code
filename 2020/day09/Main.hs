module Main where

import           Data.Maybe  (fromMaybe)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let signal = map parseUnsignedInt $ T.lines text
        invalidNumber = findInvalidNumber signal
        consecutiveGroup = findConsecutiveGroup invalidNumber signal
    print invalidNumber
    print $ minimum consecutiveGroup + maximum consecutiveGroup

findInvalidNumber :: [Int] -> Int
findInvalidNumber signal = go preamble mainContent
  where
    (preamble, mainContent) = splitAt 25 signal
    go _ [] = error "no invalid number in this signal"
    go scanned (next:remaining) =
      if next `isSumOfTwoElementsIn` scanned
        then go (drop 1 scanned <> [next]) remaining
        else next

isSumOfTwoElementsIn :: Int -> [Int] -> Bool
_ `isSumOfTwoElementsIn` [] = False
target `isSumOfTwoElementsIn` (x:xs) =
  target - x `elem` xs || target `isSumOfTwoElementsIn` xs

findConsecutiveGroup :: Int -> [Int] -> [Int]
findConsecutiveGroup = go
  where
    go _ [] = error "no consecutive segment that sums to target"
    go targetSum xs =
      fromMaybe (go targetSum $ drop 1 xs) (findInitialSegment targetSum xs)

findInitialSegment :: Int -> [Int] -> Maybe [Int]
findInitialSegment = go
  where
    go _ [] = Nothing
    go targetSum (x:xs)
      | targetSum < 0 = Nothing
      | targetSum == 0 = Just []
      | otherwise = (x :) <$> go (targetSum - x) xs
