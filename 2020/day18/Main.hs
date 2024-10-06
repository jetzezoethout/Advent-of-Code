module Main where

import qualified Data.Text   as T
import           Expression  (evaluate)
import           Parser      (parseAdvancedExpression, parseExpression)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        expressions = map parseExpression textLines
        advancedExpressions = map parseAdvancedExpression textLines
    print $ sum $ map evaluate expressions
    print $ sum $ map evaluate advancedExpressions
