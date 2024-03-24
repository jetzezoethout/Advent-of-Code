module City where

import           Coordinate (Coordinate (..))
import           Data.Char  (digitToInt)
import           Data.Text  (Text)
import           Grid       (Grid (..), parseGrid)
import           Node       (Node (..))

type City = Grid Int

parseCity :: Text -> City
parseCity = parseGrid digitToInt

isFinalNode :: Node -> City -> Bool
isFinalNode Node {..} city =
  coordinate.row == city.height - 1 && coordinate.column == city.width - 1
