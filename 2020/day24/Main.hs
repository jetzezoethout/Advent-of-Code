module Main where

import qualified Data.Text     as T
import           Floor         (blackTiles, flipTiles, runExhibit)
import           HexCoordinate (walk)
import           HexDirection  (parseHexDirections)
import           ProcessFile   (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let directions = map parseHexDirections $ T.lines text
        tilesToFlip = map walk directions
        startOfExhibit = flipTiles tilesToFlip
    print $ blackTiles startOfExhibit
    print $ blackTiles $ runExhibit 100 startOfExhibit
