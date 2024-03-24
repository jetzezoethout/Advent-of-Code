module Main where

import           Data.Foldable   (traverse_)
import           Data.List.Split (chunksOf)
import qualified Data.Text       as T
import           Instruction     (execute, parseInstruction)
import           Pixel           (draw, toChar)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let instructions = map parseInstruction $ T.lines text
        signalSegments = take 6 $ chunksOf 40 $ execute instructions
    print $ signalStrength signalSegments
    traverse_ (putStrLn . map toChar . draw) signalSegments

signalStrength :: [[Int]] -> Int
signalStrength segments =
  sum
    $ zipWith
        (\segment index -> (40 * index + 20) * (segment !! 19))
        segments
        [0 ..]
