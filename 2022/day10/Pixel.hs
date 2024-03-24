module Pixel where

data Pixel
  = Light
  | Dark
  deriving (Show)

toChar :: Pixel -> Char
toChar Light = '#'
toChar Dark  = ' '

draw :: [Int] -> [Pixel]
draw segment =
  zipWith
    (\value index ->
       if abs (value - index) <= 1
         then Light
         else Dark)
    segment
    [0 ..]
