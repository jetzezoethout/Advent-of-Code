module Main where

import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           DeclarationForm (commonAnswers, parseDeclarationForm,
                                  totalAnswers)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let formGroups =
          map (map parseDeclarationForm) $ splitOn [""] $ T.lines text
    print $ sum $ map totalAnswers formGroups
    print $ sum $ map commonAnswers formGroups
