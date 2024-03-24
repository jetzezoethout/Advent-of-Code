module Main where

import qualified Data.Text   as T
import           Directory   (allSizes)
import           Navigation  (parseNavigation, reconstructFileSystem)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let fileSystem = reconstructFileSystem $ parseNavigation $ T.lines text
        directorySizes = allSizes fileSystem
        toRemove = head directorySizes - 40000000
    print $ sum $ filter (< 100000) directorySizes
    print $ minimum $ filter (>= toRemove) directorySizes
