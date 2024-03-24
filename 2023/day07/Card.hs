module Card where

import           JokerOrd (JokerOrd (..))

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Show)

fromChar :: Char -> Card
fromChar '2' = Two
fromChar '3' = Three
fromChar '4' = Four
fromChar '5' = Five
fromChar '6' = Six
fromChar '7' = Seven
fromChar '8' = Eight
fromChar '9' = Nine
fromChar 'T' = Ten
fromChar 'J' = Jack
fromChar 'Q' = Queen
fromChar 'K' = King
fromChar 'A' = Ace
fromChar _   = error "not a card :("

instance JokerOrd Card where
  jokerCompare :: Card -> Card -> Ordering
  Jack `jokerCompare` Jack   = EQ
  Jack `jokerCompare` _      = LT
  _ `jokerCompare` Jack      = GT
  card1 `jokerCompare` card2 = card1 `compare` card2
