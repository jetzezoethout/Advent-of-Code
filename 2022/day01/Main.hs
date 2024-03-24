module Main where

import           Data.List       (sortBy)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           Inventory       (parseCalories, totalCalories)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let allInventories = map parseCalories . splitOn [""] $ T.lines text
    let sortedCalories =
          sortBy (flip compare) $ map totalCalories allInventories
    print $ head sortedCalories
    print $ sum $ take 3 sortedCalories
