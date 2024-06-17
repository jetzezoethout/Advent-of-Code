module Main where

import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Point           (manhattanDiameter)
import           ProcessFile     (processFile)
import           Scanner         (AbsoluteScanner (..), parseScanner,
                                  reconstructMap)

main :: IO ()
main =
  processFile $ \text -> do
    let scanners = map parseScanner $ splitOn [""] $ T.lines text
        absoluteScanners = reconstructMap scanners
    print
      $ S.size
      $ foldl' S.union S.empty
      $ map (S.fromList . absBeacons) absoluteScanners
    print $ manhattanDiameter $ map absScannerPosition absoluteScanners
