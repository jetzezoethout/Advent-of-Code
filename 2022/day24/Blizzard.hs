module Blizzard where

import           Coordinate (Coordinate (Coordinate))
import           Data.Maybe (catMaybes)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Direction  (Direction (..), moveTowardsBy)
import           TaggedRow  (TaggedRow (TaggedRow, content, rowIndex), zipLines)

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
parseBlizzards textLines = zipLines textLines >>= blizzardsOnRow
  where
    blizzardsOnRow TaggedRow {..} =
      catMaybes
        $ zipWith
            (\possibleDir colIndex ->
               Blizzard (Coordinate rowIndex colIndex) <$> possibleDir)
            (map blizzardDirection $ T.unpack content)
            [0 ..]

atTime :: Blizzard -> Int -> Coordinate
atTime Blizzard {..} = position `moveTowardsBy` direction
