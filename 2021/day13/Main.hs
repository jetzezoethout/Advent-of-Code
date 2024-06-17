module Main where

import           Data.List        (foldl')
import           Data.List.Split  (splitOn)
import qualified Data.Set         as S
import qualified Data.Text        as T
import           Fold             (parseFold)
import           ProcessFile      (processFile)
import           TransparentPaper (displayPaper, foldPaper,
                                   parseTransparentPaper)

main :: IO ()
main =
  processFile $ \text -> do
    let parts = splitOn [""] $ T.lines text
        paper = parseTransparentPaper $ head parts
        folds = map parseFold $ parts !! 1
    print $ S.size $ foldPaper paper $ head folds
    putStrLn $ displayPaper $ foldl' foldPaper paper folds
