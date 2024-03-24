module Inventory where

import           Data.Text (Text)
import           Parsers   (parseUnsignedInt)

newtype Inventory = Inventory
  { getCalories :: [Int]
  } deriving (Show)

parseCalories :: [Text] -> Inventory
parseCalories = Inventory . map parseUnsignedInt

totalCalories :: Inventory -> Int
totalCalories = sum . getCalories
