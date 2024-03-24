module ClonedCard where

import           Card (Card, getCorrectNumbersAmount)

data ClonedCard = ClonedCard
  { card   :: Card
  , copies :: Int
  }

single :: Card -> ClonedCard
single card = ClonedCard card 1

cascadeClones :: [ClonedCard] -> [ClonedCard]
cascadeClones [] = []
cascadeClones (firstClone@ClonedCard {..}:remainingClones) =
  firstClone : cascadeClones remainingClonesWithWonCards
  where
    remainingClonesWithWonCards =
      addClones copies (getCorrectNumbersAmount card) remainingClones

addClones :: Int -> Int -> [ClonedCard] -> [ClonedCard]
addClones _ 0 clones = clones
addClones _ _ [] = []
addClones clonesToAdd cloneRange (ClonedCard {..}:remainingClones) =
  ClonedCard {card = card, copies = copies + clonesToAdd}
    : addClones clonesToAdd (cloneRange - 1) remainingClones
