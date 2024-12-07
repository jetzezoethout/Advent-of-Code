module RockContainers where

import           Coordinate  (Coordinate (..))
import           Data.Set    (Set, fromList)
import           Data.Text   (Text)
import           Grid        (Grid, parseGrid)
import           LocatedChar (LocatedChar (..), locateText)

type FixedRockTeller = Grid Bool

type RollingRocks = Set Coordinate

parseFixedRocks :: Text -> Grid Bool
parseFixedRocks = parseGrid (== '#')

parseRollingRocks :: Text -> RollingRocks
parseRollingRocks =
  fromList . map location . filter ((== 'O') . char) . locateText
