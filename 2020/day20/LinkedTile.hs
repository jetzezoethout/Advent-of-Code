module LinkedTile where

import           Coordinate  (Coordinate (column, row))
import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Maybe  (listToMaybe, mapMaybe)
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Direction   (Direction (..), allDirections)
import           Reflectable (Reflectable (reflectRows))
import           Rotatable   (Rotatable (..))
import           Tile        (Tile (..), TileID)

data LinkedTile = LinkedTile
  { tile  :: Tile
  , links :: Map Direction TileID
  } deriving (Show)

instance Reflectable LinkedTile where
  reflectRows :: LinkedTile -> LinkedTile
  reflectRows LinkedTile {..} =
    LinkedTile {tile = reflectRows tile, links = M.mapKeys reflectRows links}

instance Rotatable LinkedTile where
  rotateClockWise :: LinkedTile -> LinkedTile
  rotateClockWise LinkedTile {..} =
    LinkedTile
      {tile = rotateClockWise tile, links = M.mapKeys rotateClockWise links}

isCornerTile :: LinkedTile -> Bool
isCornerTile LinkedTile {..} = M.size links == 2

newtype Edge =
  Edge (Set Int)

instance Eq Edge where
  (==) :: Edge -> Edge -> Bool
  Edge edge1 == Edge edge2 = edge1 == edge2 || edge1 == S.map (9 -) edge2

edgeAt :: Tile -> Direction -> Edge
(Tile tile) `edgeAt` North = Edge $ S.map column $ S.filter ((== 0) . row) tile
(Tile tile) `edgeAt` East  = Edge $ S.map row $ S.filter ((== 9) . column) tile
(Tile tile) `edgeAt` South = Edge $ S.map column $ S.filter ((== 9) . row) tile
(Tile tile) `edgeAt` West  = Edge $ S.map row $ S.filter ((== 0) . column) tile

edgesOf :: Tile -> [Edge]
edgesOf tile = map (tile `edgeAt`) allDirections

linkTiles :: Map TileID Tile -> Map TileID LinkedTile
linkTiles tiles = M.mapWithKey linkTile tiles
  where
    edges = M.map edgesOf tiles
    linkTile tileID tile =
      LinkedTile
        { tile = tile
        , links = M.fromList $ mapMaybe (getLink tileID tile) allDirections
        }
    getLink tileID tile dir = do
      let edge = tile `edgeAt` dir
      fittingTile <-
        listToMaybe $ M.keys $ M.filter (elem edge) $ M.delete tileID edges
      return (dir, fittingTile)
