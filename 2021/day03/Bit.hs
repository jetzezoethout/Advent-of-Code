module Bit where

import           Data.Foldable (Foldable (foldl'))

data Bit
  = Zero
  | One
  deriving (Eq, Show)

fromChar :: Char -> Bit
fromChar '0' = Zero
fromChar '1' = One
fromChar _   = error "not a bit"

invertBit :: Bit -> Bit
invertBit Zero = One
invertBit One  = Zero

invert :: [Bit] -> [Bit]
invert = map invertBit

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1

toInt :: [Bit] -> Int
toInt = foldl' (\acc bit -> 2 * acc + bitToInt bit) 0
