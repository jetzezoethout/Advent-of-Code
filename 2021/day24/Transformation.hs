module Transformation where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

data Transformation
    -- Represents a transformation of the form:
    -- \input z -> input + constant + 26z
  = Linear
      { constant :: Int
      }
    -- Represents a transformation of the form:
    -- \input z -> if 26 | z - input + offset
    --   then z div 26
    --   else input + constant + rounded + 26 * (z div 26)
    -- Note that constant is not included because we don´t need it
  | Modulo26
      { offset :: Int
      }
  deriving (Show)

parseTransformation :: [Text] -> Transformation
parseTransformation textLines =
  if textLines !! 4 == "div z 1"
    then Linear {constant = extractParameter $ textLines !! 15}
    else Modulo26 {offset = extractParameter $ textLines !! 5}
  where
    extractParameter textLine = parseInt $ T.words textLine !! 2

data IntermediateState = IntermediateState
  { input  :: Int
  , zValue :: Int
  } deriving (Show)

reverseEngineer :: Transformation -> Int -> [IntermediateState]
reverseEngineer Linear {..} targetZ =
  [ IntermediateState inp z
  | inp <- [1 .. 9]
  , let twentySixZ = targetZ - inp - constant
  , twentySixZ `mod` 26 == 0
  , z <- [twentySixZ `div` 26]
  ]
reverseEngineer Modulo26 {..} targetZ
  -- There are actually other possibilities here, but fortunately, MONAD doesn't
  -- use them :)
 =
  [ IntermediateState inp z
  | inp <- [1 .. 9]
  , z <- [26 * targetZ .. 26 * targetZ + 25]
  , (z - inp + offset) `mod` 26 == 0
  ]

allModelNumbers :: [Transformation] -> [Int]
allModelNumbers instructions = go (reverse instructions) [] 0
  where
    toInt :: [Int] -> Int
    toInt = foldl (\a b -> 10 * a + b) 0
    go :: [Transformation] -> [Int] -> Int -> [Int]
    go [] inputSequence z = [toInt inputSequence | z == 0]
    go (lastTransformation:previousTransformations) inputSequence z = do
      IntermediateState {..} <- reverseEngineer lastTransformation z
      go previousTransformations (input : inputSequence) zValue
