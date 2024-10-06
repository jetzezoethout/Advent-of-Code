module Instruction where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

data Operation
  = Acc
  | Jump
  | NoOp

parseOperation :: Text -> Operation
parseOperation "acc" = Acc
parseOperation "jmp" = Jump
parseOperation "nop" = NoOp
parseOperation _     = error "unknown operation"

flipOperation :: Operation -> Maybe Operation
flipOperation Jump = Just NoOp
flipOperation NoOp = Just Jump
flipOperation Acc  = Nothing

data Instruction = Instruction
  { operation :: Operation
  , argument  :: Int
  }

parseInstruction :: Text -> Instruction
parseInstruction text =
  let parts = T.words text
   in Instruction
        { operation = parseOperation $ head parts
        , argument = parseInt $ parts !! 1
        }

updateAddress :: Instruction -> Int -> Int
updateAddress Instruction {..} =
  (+ case operation of
       Jump -> argument
       _    -> 1)

updateAccumulator :: Instruction -> Int -> Int
updateAccumulator Instruction {..} =
  (+ case operation of
       Acc -> argument
       _   -> 0)

repairInstruction :: Instruction -> Maybe Instruction
repairInstruction Instruction {..} = do
  flippedOperation <- flipOperation operation
  return $ Instruction {operation = flippedOperation, argument = argument}
