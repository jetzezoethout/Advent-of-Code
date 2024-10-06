module Tile where

import           Coordinate      (Coordinate (..), addCoordinate)
import           Data.List.Split (splitOn)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Parsers         (parseUnsignedInt)
import           Reflectable     (Reflectable (reflectRows))
import           Rotatable
import           TaggedRow       (TaggedRow (..), zipLines)

type TileID = Int

newtype Tile =
  Tile (Set Coordinate)
  deriving (Show)

move :: Coordinate -> Tile -> Set Coordinate
move delta (Tile squares) = S.map (addCoordinate delta) squares

instance Reflectable Tile where
  reflectRows :: Tile -> Tile
  reflectRows (Tile squares) = Tile $ S.map reflectSquare squares
    where
      reflectSquare Coordinate {..} =
        Coordinate {row = 9 - row, column = column}

instance Rotatable Tile where
  rotateClockWise :: Tile -> Tile
  rotateClockWise (Tile squares) = Tile $ S.map rotateSquare squares
    where
      rotateSquare Coordinate {..} = Coordinate {row = column, column = 9 - row}

parseTile :: [Text] -> Tile
parseTile textLines = Tile $ S.fromList $ zipLines textLines >>= processRow
  where
    processRow TaggedRow {..} =
      map (Coordinate rowIndex . fst)
        $ filter ((== '#') . snd)
        $ zip [0 ..]
        $ T.unpack content

parseTiles :: Text -> Map TileID Tile
parseTiles = M.fromList . map parseTileAndID . splitOn [""] . T.lines
  where
    parseTileAndID textLines =
      ( parseUnsignedInt $ T.init $ T.drop 5 $ head textLines
      , parseTile $ tail textLines)
