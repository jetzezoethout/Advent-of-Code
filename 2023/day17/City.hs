module City where

import           Coordinate (Coordinate (..))
import           Data.Char  (digitToInt)
import           Data.Text  (Text)
import           Grid       (Grid (..), parseGrid)
import           Node       (Node (..), Orientation (Horizontal, Vertical))

type City = Grid Int

parseCity :: Text -> City
parseCity = parseGrid digitToInt

isFinalNode :: Node -> City -> Bool
isFinalNode Node {..} city =
  coordinate.row == city.height - 1 && coordinate.column == city.width - 1

allNodesAsc :: City -> [Node]
allNodesAsc city =
  [ Node (Coordinate r c) orient
  | r <- [0 .. city.height - 1]
  , c <- [0 .. city.width - 1]
  , orient <- [Horizontal, Vertical]
  ]
