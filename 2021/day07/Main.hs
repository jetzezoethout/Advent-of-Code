module Main where

import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let crabs = map parseUnsignedInt $ T.splitOn "," text
        crabRange = (minimum crabs, maximum crabs)
    print $ findConvexMin (totalFuel wrongFuel crabs) crabRange
    print $ findConvexMin (totalFuel correctFuel crabs) crabRange

wrongFuel :: Int -> Int
wrongFuel = abs

correctFuel :: Integral a => a -> a
correctFuel distance = (abs distance * (abs distance + 1)) `div` 2

totalFuel :: (Int -> Int) -> [Int] -> Int -> Int
totalFuel fuelCost crabs target = sum $ map (fuelCost . (target -)) crabs

-- Minimize a *convex* function on a given range
findConvexMin :: (Int -> Int) -> (Int, Int) -> Int
findConvexMin f (left, right)
  | f (probe - 1) < sample = findConvexMin f (left, probe - 1)
  | f (probe + 1) < sample = findConvexMin f (probe + 1, right)
  | otherwise = sample
  where
    probe = (left + right) `div` 2
    sample = f probe
