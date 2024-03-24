module Main where

import           Data.List.Split (chunksOf)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Rucksack        (commonItem, duplicateItem, itemScore,
                                  parseRuckSack)

main :: IO ()
main =
  processFile $ \text -> do
    let rucksacks = map parseRuckSack $ T.lines text
        groups = chunksOf 3 rucksacks
    print $ sum $ map (itemScore . duplicateItem) rucksacks
    print $ sum $ map (itemScore . commonItem) groups
