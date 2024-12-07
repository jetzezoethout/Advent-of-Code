module Deck where

import           Data.Text (Text)
import           Parsers   (parseUnsignedInt)

type Card = Int

newtype Deck = Deck
  { cards :: [Card]
  } deriving (Show, Eq, Ord)

data Play = Play
  { cardPlayed :: Card
  , left       :: Deck
  }

parseDeck :: [Text] -> Deck
parseDeck textLines = Deck $ map parseUnsignedInt $ tail textLines

play :: Deck -> Maybe Play
play (Deck cards) =
  case cards of
    [] -> Nothing
    topCard:cardsLeft ->
      Just $ Play {cardPlayed = topCard, left = Deck cardsLeft}

createSubDeck :: Play -> Maybe Deck
createSubDeck Play {..} =
  [Deck $ take cardPlayed $ cards left | cardPlayed <= length (cards left)]

insertOnBottom :: Deck -> [Card] -> Deck
insertOnBottom (Deck cards) newCards = Deck $ cards <> newCards

score :: Deck -> Int
score (Deck cards) = sum $ zipWith (*) [1 ..] $ reverse cards
