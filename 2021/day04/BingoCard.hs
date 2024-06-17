module BingoCard where

import           BingoNumber (BingoNumber (..), isMarked, mark)
import           Data.List   (find)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Parsers     (parseUnsignedInt)

newtype BingoCard = BingoCard
  { getCard :: [[BingoNumber]]
  } deriving (Show)

parseBingoCard :: [Text] -> BingoCard
parseBingoCard = BingoCard . map parseRow
  where
    parseRow = map (Unmarked . parseUnsignedInt) . T.words

checkRow :: BingoCard -> Int -> Bool
checkRow BingoCard {..} rowIndex = all isMarked $ getCard !! rowIndex

checkColumn :: BingoCard -> Int -> Bool
checkColumn BingoCard {..} colIndex =
  all (\row -> isMarked $ row !! colIndex) getCard

announceNumber :: Int -> BingoCard -> BingoCard
announceNumber number BingoCard {..} =
  BingoCard $ map (map (mark number)) getCard

hasWon :: BingoCard -> Bool
hasWon bingoCard =
  any (checkRow bingoCard) [0 .. 4] || any (checkColumn bingoCard) [0 .. 4]

unmarkedSum :: BingoCard -> Int
unmarkedSum BingoCard {..} = sum $ map sumRow getCard
  where
    sumRow = sum . map value . filter (not . isMarked)

winningScore :: [Int] -> [BingoCard] -> Int
winningScore = go
  where
    go [] _ = error "no winning card!"
    go (nextNumber:others) cards =
      let newCards = map (announceNumber nextNumber) cards
       in case find hasWon newCards of
            Nothing          -> go others newCards
            Just winningCard -> nextNumber * unmarkedSum winningCard

losingScore :: [Int] -> [BingoCard] -> Int
losingScore = go
  where
    go _ [] = error "no unique losing card!"
    go [] _ = error "not all cards end up winning!"
    go numbers [candidate] = finishCard numbers candidate
    go (nextNumber:others) candidates =
      go others
        $ filter (not . hasWon)
        $ map (announceNumber nextNumber) candidates
    finishCard [] _ = error "this card cannot win!"
    finishCard (nextNumber:others) card =
      let updatedCard = announceNumber nextNumber card
       in if hasWon updatedCard
            then nextNumber * unmarkedSum updatedCard
            else finishCard others updatedCard
