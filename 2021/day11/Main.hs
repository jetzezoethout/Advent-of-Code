module Main where

import           Data.List     (elemIndex)
import           Data.Maybe    (fromJust)
import           OctopusCavern (flashForever, parseOctopusCavern)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let flashes = flashForever $ parseOctopusCavern text
    print $ sum $ take 100 flashes
    print $ fromJust (elemIndex 100 flashes) + 1
