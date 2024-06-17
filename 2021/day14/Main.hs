module Main where

import qualified Data.Text       as T
import           Frequencies     (analysis)
import           InsertionRule   (parseInsertionRules)
import           PolymerTemplate (expandPolymerTemplate)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        polymerTemplate = T.unpack $ head textLines
        insertionRules = parseInsertionRules $ drop 2 textLines
    print $ analysis $ expandPolymerTemplate insertionRules polymerTemplate 10
    print $ analysis $ expandPolymerTemplate insertionRules polymerTemplate 40
