module Move where

import           Outcome (Outcome)

data Move
  = Rock
  | Paper
  | Scissors
  deriving (Eq, Show, Enum)

parseMove :: Char -> Move
parseMove 'A' = Rock
parseMove 'X' = Rock
parseMove 'B' = Paper
parseMove 'Y' = Paper
parseMove 'C' = Scissors
parseMove 'Z' = Scissors
parseMove _   = error "Not a valid move"

moveScore :: Move -> Int
moveScore = (+ 1) . fromEnum

versus :: Move -> Move -> Outcome
ours `versus` theirs = toEnum $ (fromEnum ours - fromEnum theirs + 1) `mod` 3

against :: Outcome -> Move -> Move
goal `against` theirs = toEnum $ (fromEnum theirs + fromEnum goal - 1) `mod` 3
