module Expression where

data Operator
  = Plus
  | Times
  deriving (Show)

applyOperator :: Operator -> Int -> Int -> Int
applyOperator Plus  = (+)
applyOperator Times = (*)

data Expression
  = Atomic Int
  | Complex
      { left     :: Expression
      , operator :: Operator
      , right    :: Expression
      }
  deriving (Show)

evaluate :: Expression -> Int
evaluate (Atomic x)   = x
evaluate Complex {..} = applyOperator operator (evaluate left) (evaluate right)
