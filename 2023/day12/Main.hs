module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           RowData     (RowData (..), getPossibilities, parseRowData,
                              unfold)

main :: IO ()
main =
  processFile $ \text -> do
    let rowData = map parseRowData $ T.lines text
        unfoldedData = map unfold rowData
    print $ getAnswer rowData
    print $ getAnswer unfoldedData

getAnswer :: [RowData] -> Int
getAnswer = sum . map getPossibilities
