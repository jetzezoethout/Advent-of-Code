module Main where

import           Data.List   (nub)
import qualified Data.Text   as T
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let signal = T.unpack text
    print $ findMarker 4 signal
    print $ findMarker 14 signal

type Signal = [Char]

hasDuplicates :: Signal -> Bool
hasDuplicates signal = length signal == length (nub signal)

findMarker :: Int -> Signal -> Int
findMarker markerSize = go markerSize
  where
    go charsRead remainingSignal =
      if hasDuplicates (take markerSize remainingSignal)
        then charsRead
        else go (charsRead + 1) (tail remainingSignal)
