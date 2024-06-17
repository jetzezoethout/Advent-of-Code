module Main where

import           Data.List   (sort)
import           Data.Maybe  (mapMaybe)
import qualified Data.Text   as T
import           Interpreter (autoCompleteScore, illegalityScore, interpret)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let interpreterResults = map interpret $ T.lines text
    print $ sum $ mapMaybe illegalityScore interpreterResults
    print $ median $ mapMaybe autoCompleteScore interpreterResults

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)
