module Main where

import qualified Data.Text   as T
import           ProcessFile (processFile)
import           Snafu       (fromSnafu, toSnafu)

main :: IO ()
main =
  processFile $ \text -> do
    let snafuNumbers = map T.unpack $ T.lines text
    putStrLn $ toSnafu $ sum $ map fromSnafu snafuNumbers
    putStrLn "Start the blender!"
