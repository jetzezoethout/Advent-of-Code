module Main where

import           CraneType       (CraneType (..))
import           Crates          (parseCrates, processInstructions, topCrates)
import           Data.List.Split (splitOn)
import qualified Data.Text       as T
import           MoveInstruction (parseMoveInstruction)
import           ProcessFile     (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let textLines = T.lines text
        parts = splitOn [""] textLines
        initialCrates = parseCrates $ head parts
        instructions = map parseMoveInstruction $ parts !! 1
    putStrLn
      $ topCrates
      $ processInstructions CrateMover9000 initialCrates instructions
    putStrLn
      $ topCrates
      $ processInstructions CrateMover9001 initialCrates instructions
