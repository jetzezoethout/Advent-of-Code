module Outcome where

data Outcome
  = Lose
  | Draw
  | Win
  deriving (Show, Eq, Enum)

outcomeScore :: Outcome -> Int
outcomeScore = (* 3) . fromEnum

parseOutcome :: Char -> Outcome
parseOutcome 'X' = Lose
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win
parseOutcome _   = error "not a valid outcome"
