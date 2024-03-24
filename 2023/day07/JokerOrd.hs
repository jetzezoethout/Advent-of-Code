module JokerOrd where

class Eq a =>
      JokerOrd a
  where
  jokerCompare :: a -> a -> Ordering

instance JokerOrd a => JokerOrd [a] where
  jokerCompare :: JokerOrd a => [a] -> [a] -> Ordering
  [] `jokerCompare` []         = EQ
  [] `jokerCompare` _          = LT
  _ `jokerCompare` []          = GT
  (x:xs) `jokerCompare` (y:ys) = (x `jokerCompare` y) <> (xs `jokerCompare` ys)
