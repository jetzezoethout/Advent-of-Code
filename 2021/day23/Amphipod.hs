module Amphipod where

data Amphipod
  = Amber
  | Bronze
  | Copper
  | Desert
  deriving (Eq, Ord, Show, Enum)

fromChar :: Char -> Amphipod
fromChar 'A' = Amber
fromChar 'B' = Bronze
fromChar 'C' = Copper
fromChar 'D' = Desert
fromChar _   = error "Unknown amphipod"

targetColumn :: Amphipod -> Int
targetColumn amphipod = 2 * (fromEnum amphipod + 1)

allAmphipods :: [Amphipod]
allAmphipods = [Amber .. Desert]

energy :: Amphipod -> Int
energy amphipod = 10 ^ fromEnum amphipod
