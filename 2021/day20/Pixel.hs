module Pixel where

import           Data.List (foldl')

data Pixel
  = Light
  | Dark
  deriving (Eq, Show)

fromChar :: Char -> Pixel
fromChar '.' = Dark
fromChar '#' = Light
fromChar _   = error "not a pixel"

binaryValue :: Pixel -> Int
binaryValue Dark  = 0
binaryValue Light = 1

toIndex :: [Pixel] -> Int
toIndex = foldl' (\acc pixel -> 2 * acc + binaryValue pixel) 0
