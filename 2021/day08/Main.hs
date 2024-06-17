module Main where

import           BrokenDisplay (easyOnes, parseBrokenDisplay, repair)
import qualified Data.Text     as T
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let brokenDisplays = map parseBrokenDisplay $ T.lines text
    print $ sum $ map easyOnes brokenDisplays
    print $ sum $ map repair brokenDisplays
