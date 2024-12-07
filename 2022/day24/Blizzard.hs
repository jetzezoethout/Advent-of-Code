module Blizzard where

import           Coordinate  (Coordinate)
import           Data.Maybe  (mapMaybe)
import           Data.Text   (Text)
import           Direction   (Direction (..), moveTowardsBy)
import           LocatedChar (LocatedChar (..), locateTextLines)

data Blizzard = Blizzard
  { position  :: Coordinate
  , direction :: Direction
  } deriving (Show)

blizzardDirection :: Char -> Maybe Direction
blizzardDirection '^' = Just North
blizzardDirection '>' = Just East
blizzardDirection 'v' = Just South
blizzardDirection '<' = Just West
blizzardDirection '.' = Nothing
blizzardDirection _   = error "don't know what this is"

parseBlizzards :: [Text] -> [Blizzard]
parseBlizzards = mapMaybe findBlizzard . locateTextLines
  where
    findBlizzard LocatedChar {..} = Blizzard location <$> blizzardDirection char

atTime :: Blizzard -> Int -> Coordinate
atTime Blizzard {..} = position `moveTowardsBy` direction
