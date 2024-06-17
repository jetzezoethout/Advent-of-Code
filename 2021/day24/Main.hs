module Main where

import           Data.List.Split (chunksOf)
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           Transformation  (allModelNumbers, parseTransformation)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        transformations = map parseTransformation $ chunksOf 18 textLines
        modelNumbers = allModelNumbers transformations
    print $ maximum modelNumbers
    print $ minimum modelNumbers
