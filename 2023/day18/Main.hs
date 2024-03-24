module Main where

import qualified Data.Text         as T
import           Polygon           (makePolygon, totalPoints)
import           ProcessFile       (processFile)
import           TrenchInstruction (parseTrenchInstructions)

main :: IO ()
main =
  processFile $ \text -> do
    let (instructions, hexInstructions) =
          unzip $ map parseTrenchInstructions $ T.lines text
        polygon = makePolygon instructions
        hexPolygon = makePolygon hexInstructions
    print $ totalPoints polygon
    print $ totalPoints hexPolygon
