module Main where

import qualified Data.Text   as T
import           History     (History, extrapolate, parseHistory,
                              reverseHistory)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let histories = map parseHistory $ T.lines text
    print $ getAnswer1 histories
    print $ getAnswer2 histories

getAnswer1 :: [History] -> Int
getAnswer1 = sum . map extrapolate

getAnswer2 :: [History] -> Int
getAnswer2 = sum . map (extrapolate . reverseHistory)
