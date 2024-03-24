module Instruction where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

data Instruction
  = Addx Int
  | Noop
  deriving (Show)

parseInstruction :: Text -> Instruction
parseInstruction text =
  if text == "noop"
    then Noop
    else Addx (parseInt $ T.words text !! 1)

execute :: [Instruction] -> [Int]
execute = executeFrom 1

executeFrom :: Int -> [Instruction] -> [Int]
executeFrom currentValue [] = repeat currentValue
executeFrom currentValue (nextInstruction:remaining) =
  case nextInstruction of
    Noop -> currentValue : executeFrom currentValue remaining
    Addx diff ->
      currentValue : currentValue : executeFrom (currentValue + diff) remaining
