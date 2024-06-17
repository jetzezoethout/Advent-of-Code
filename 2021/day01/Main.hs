module Main where

import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let measurements = map parseUnsignedInt $ T.lines text
    print $ numberOfIncreased measurements
    print $ numberOfIncreased $ windowsOfThree measurements

numberOfIncreased :: Ord b => [b] -> Int
numberOfIncreased xs = length $ filter (== LT) $ zipWith compare xs $ tail xs

windowsOfThree :: Num a => [a] -> [a]
windowsOfThree xs = sumShifted xs $ sumShifted xs xs
  where
    sumShifted :: Num a => [a] -> [a] -> [a]
    sumShifted ys zs = zipWith (+) ys $ tail zs
