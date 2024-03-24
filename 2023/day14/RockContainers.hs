module RockContainers where

import           Coordinate (Coordinate (..))
import           Data.Set   (Set, fromList)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Grid       (Grid, parseGrid)
import           TaggedRow  (TaggedRow (..), parseTaggedLines)

type FixedRockTeller = Grid Bool

type RollingRocks = Set Coordinate

parseFixedRocks :: Text -> Grid Bool
parseFixedRocks = parseGrid (== '#')

parseRollingRocks :: Text -> RollingRocks
parseRollingRocks text = fromList $ map fst $ filter ((== 'O') . snd) pairs
  where
    taggedLines = parseTaggedLines text
    pairsOnRow TaggedRow {..} =
      zipWith (\colIndex char -> (Coordinate rowIndex colIndex, char)) [0 ..]
        $ T.unpack content
    pairs = taggedLines >>= pairsOnRow
