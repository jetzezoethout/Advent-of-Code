module Main where

import qualified Data.Text   as T
import           Game        (playRounds)
import           Parsers     (parseUnsignedInt)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let initialNumbers = map parseUnsignedInt $ T.splitOn "," text
    print $ playRounds 2020 initialNumbers
    print $ playRounds 30000000 initialNumbers
