module InspectionRule where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data InspectionRule = InspectionRule
  { operation     :: Int -> Int
  , getNextMonkey :: Int -> Int
  }

parseOperation :: Text -> Int -> Int
parseOperation text =
  let expression = T.splitOn " = " text !! 1
      expressionParts = T.words expression
      leftInput = parseOperationInput $ head expressionParts
      operator = parseOperator $ expressionParts !! 1
      rightInput = parseOperationInput $ expressionParts !! 2
   in \worryLevel -> leftInput worryLevel `operator` rightInput worryLevel

parseOperationInput :: Text -> Int -> Int
parseOperationInput "old"  = id
parseOperationInput number = const $ parseUnsignedInt number

parseOperator :: Text -> Int -> Int -> Int
parseOperator "+" = (+)
parseOperator "*" = (*)
parseOperator _   = error "not an operator"

parseFinalNumber :: Text -> Int
parseFinalNumber = parseUnsignedInt . last . T.words

parseGetNextMonkey :: [Text] -> Int -> Int
parseGetNextMonkey textLines =
  let divisor = parseFinalNumber $ head textLines
      monkeyIfTrue = parseFinalNumber $ textLines !! 1
      monkeyIfFalse = parseFinalNumber $ textLines !! 2
   in \worryLevel ->
        if worryLevel `mod` divisor == 0
          then monkeyIfTrue
          else monkeyIfFalse

parseInspectionRule :: [Text] -> InspectionRule
parseInspectionRule textLines =
  InspectionRule
    { operation = parseOperation $ head textLines
    , getNextMonkey = parseGetNextMonkey $ tail textLines
    }

amendOperation :: (Int -> Int) -> InspectionRule -> InspectionRule
amendOperation postOp InspectionRule {..} =
  InspectionRule {operation = postOp . operation, getNextMonkey = getNextMonkey}
