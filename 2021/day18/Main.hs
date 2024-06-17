module Main where

import           Data.List       (foldl1')
import qualified Data.Text       as T
import           ProcessFile     (processFile)
import           SnailfishNumber (add, magnitude, parseSnailfishNumber)

main :: IO ()
main =
  processFile $ \text -> do
    let snailfishNumbers = map parseSnailfishNumber $ T.lines text
    print $ magnitude $ foldl1' add snailfishNumbers
    print
      $ maximum
      $ [ magnitude (nr1 `add` nr2)
        | nr1 <- snailfishNumbers
        , nr2 <- snailfishNumbers
        ]
