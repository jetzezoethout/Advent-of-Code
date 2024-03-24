module Hand where

import           Card          (Card (Jack), fromChar)
import           Data.Function (on)
import           Data.List     (group, sort, sortBy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           JokerOrd      (JokerOrd (jokerCompare))

newtype Hand = Hand
  { getHand :: [Card]
  } deriving (Eq, Show)

parseHand :: Text -> Hand
parseHand = Hand . map fromChar . T.unpack

getGroupSizesDesc :: Hand -> [Int]
getGroupSizesDesc = sortDesc . map length . group . sort . getHand

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy $ flip compare

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare = (compare `on` getGroupSizesDesc) <> (compare `on` getHand)

getJackCount :: Hand -> Int
getJackCount = length . filter (== Jack) . getHand

getNonJackGroupSizesDesc :: Hand -> [Int]
getNonJackGroupSizesDesc =
  sortDesc . map length . group . sort . filter (/= Jack) . getHand

getGroupSizesDescWithJokerRule :: Hand -> [Int]
getGroupSizesDescWithJokerRule hand =
  case getNonJackGroupSizesDesc hand of
    []                 -> [jokers]
    (biggestSize:rest) -> biggestSize + jokers : rest
  where
    jokers = getJackCount hand

instance JokerOrd Hand where
  jokerCompare :: Hand -> Hand -> Ordering
  jokerCompare =
    (compare `on` getGroupSizesDescWithJokerRule) <> (jokerCompare `on` getHand)
