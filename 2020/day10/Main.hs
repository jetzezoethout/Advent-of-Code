module Main where

import           Data.List   (sort)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let joltageRatings = sort $ map parseUnsignedInt $ T.lines text
        differences = differentials joltageRatings
    print $ count 1 differences * (count 3 differences + 1)
    print $ adapterArrangements joltageRatings

differentials :: [Int] -> [Int]
differentials joltageRatings = zipWith (-) joltageRatings $ 0 : joltageRatings

count :: Eq a => a -> [a] -> Int
count target = length . filter (== target)

adapterArrangements :: [Int] -> Int
adapterArrangements = go 1 [1]
  where
    go _ cache [] = head cache
    go target cache adapters@(adapter:remaining) =
      if target == adapter
        then go (target + 1) (sum (take 3 cache) : cache) remaining
        else go (target + 1) (0 : cache) adapters
