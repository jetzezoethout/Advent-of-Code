module City where

import           Coordinate (Coordinate (..))
import           Data.Char  (digitToInt)
import           Data.Text  (Text)
import           Grid       (Grid (..), parseGrid)
import           Node       (Node (..))

type City = Grid Int

parseCity :: Text -> City
parseCity = parseGrid digitToInt

isFinalNode :: City -> Node -> Bool
isFinalNode city Node {..} =
  coordinate.row == city.height - 1 && coordinate.column == city.width - 1
