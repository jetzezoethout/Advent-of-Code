module Cave where

import           Coordinate     (Coordinate (..))
import           Data.Text      (Text)
import           Direction      (Direction (..))
import           Grid           (Grid (height, width), parseGrid)
import           LightDeflector (LightDeflector, fromChar)

type Cave = Grid (Maybe LightDeflector)

parseCave :: Text -> Cave
parseCave = parseGrid fromChar

possibleEntryPairs :: Cave -> [(Coordinate, Direction)]
possibleEntryPairs cave =
  map (\row -> (Coordinate row 0, East)) [0 .. cave.height - 1]
    <> map
         (\row -> (Coordinate row (cave.width - 1), West))
         [0 .. cave.height - 1]
    <> map (\col -> (Coordinate 0 col, South)) [0 .. cave.width - 1]
    <> map
         (\col -> (Coordinate (cave.height - 1) col, North))
         [0 .. cave.width - 1]
