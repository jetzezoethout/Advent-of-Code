module Rucksack where

import           Data.Char (isUpper)
import           Data.List (foldl1', intersect)
import           Data.Text (Text)
import qualified Data.Text as T

type Item = Char

itemScore :: Item -> Int
itemScore item
  | isUpper item = fromEnum item - 38
  | otherwise = fromEnum item - 96

data Rucksack = Rucksack
  { allItems             :: [Item]
  , firstCompartmentSize :: Int
  } deriving (Show)

parseRuckSack :: Text -> Rucksack
parseRuckSack text = Rucksack (T.unpack text) (T.length text `div` 2)

firstCompartment :: Rucksack -> [Item]
firstCompartment Rucksack {..} = take firstCompartmentSize allItems

secondCompartment :: Rucksack -> [Item]
secondCompartment Rucksack {..} = drop firstCompartmentSize allItems

duplicateItem :: Rucksack -> Item
duplicateItem rucksack =
  head $ firstCompartment rucksack `intersect` secondCompartment rucksack

commonItem :: [Rucksack] -> Item
commonItem rucksacks = head $ foldl1' intersect (map allItems rucksacks)
