module Segment where

import           Data.List (sort)

data Segment
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Show, Eq, Ord, Bounded, Enum)

allSegments :: [Segment]
allSegments = [minBound .. maxBound]

fromChar :: Char -> Segment
fromChar 'a' = A
fromChar 'b' = B
fromChar 'c' = C
fromChar 'd' = D
fromChar 'e' = E
fromChar 'f' = F
fromChar 'g' = G
fromChar _   = error "not a segment"

toDigit :: [Segment] -> Int
toDigit segments =
  case sort segments of
    [A, B, C, E, F, G]    -> 0
    [C, F]                -> 1
    [A, C, D, E, G]       -> 2
    [A, C, D, F, G]       -> 3
    [B, C, D, F]          -> 4
    [A, B, D, F, G]       -> 5
    [A, B, D, E, F, G]    -> 6
    [A, C, F]             -> 7
    [A, B, C, D, E, F, G] -> 8
    [A, B, C, D, F, G]    -> 9
    _                     -> error "not a number"
