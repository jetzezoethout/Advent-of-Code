module Main where

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let expenseReport = parseExpenseReport text
    print $ head [x * y | (x, y) <- pairs expenseReport, x + y == 2020]
    print
      $ head [x * y * z | (x, y, z) <- triples expenseReport, x + y + z == 2020]

parseExpenseReport :: Text -> [Int]
parseExpenseReport = map parseUnsignedInt . T.lines

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = map (x, ) xs <> pairs xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples (x:xs) = map (merge x) (pairs xs) <> triples xs
  where
    merge p (q, r) = (p, q, r)
