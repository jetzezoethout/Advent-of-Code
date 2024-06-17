module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           SeaFloor    (Dimensions (Dimensions), moveUntilStop,
                              parseSeaFloor)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        dimensions = Dimensions (length textLines) (T.length $ head textLines)
        seaFloor = parseSeaFloor textLines
    print $ moveUntilStop dimensions seaFloor
    putStrLn "Boost the signal!"
