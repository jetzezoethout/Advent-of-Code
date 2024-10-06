module Instruction where

import           Data.Text (Text)
import qualified Data.Text as T
import           Direction
import           Parsers   (parseUnsignedInt)

data Instruction
  = MoveTowards
      { direction :: Direction
      , amount    :: Int
      }
  | MoveForward
      { amount :: Int
      }
  | TurnClockWise
  | TurnCounterClockWise
  | TurnAround
  deriving (Show)

parseInstruction :: Text -> Instruction
parseInstruction text =
  let action = T.head text
      value = parseUnsignedInt $ T.tail text
   in case (action, value) of
        ('N', amount) -> MoveTowards North amount
        ('S', amount) -> MoveTowards South amount
        ('E', amount) -> MoveTowards East amount
        ('W', amount) -> MoveTowards West amount
        ('F', amount) -> MoveForward amount
        ('L', 90)     -> TurnCounterClockWise
        ('L', 180)    -> TurnAround
        ('L', 270)    -> TurnClockWise
        ('R', 90)     -> TurnClockWise
        ('R', 180)    -> TurnAround
        ('R', 270)    -> TurnCounterClockWise
        _             -> error "invalid instruction"
