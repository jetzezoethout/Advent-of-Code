module Operation where

import           Data.Text (Text)

data Operation
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Show)

parseOperation :: Text -> Operation
parseOperation "+" = Plus
parseOperation "-" = Minus
parseOperation "*" = Times
parseOperation "/" = Divide
parseOperation _   = error "unknown operation"

execute :: Operation -> Int -> Int -> Int
execute Plus   = (+)
execute Minus  = (-)
execute Times  = (*)
execute Divide = div

solveLeft :: Operation -> Int -> Int -> Int
solveLeft Plus   = execute Minus
solveLeft Minus  = execute Plus
solveLeft Times  = execute Divide
solveLeft Divide = execute Times

solveRight :: Operation -> Int -> Int -> Int
solveRight Plus   = execute Minus
solveRight Minus  = flip $ execute Minus
solveRight Times  = execute Divide
solveRight Divide = flip $ execute Divide
