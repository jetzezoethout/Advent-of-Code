module Floor where

import           Data.Map      (Map, (!?))
import qualified Data.Map      as M
import           HexCoordinate (HexCoordinate, neighbours)

data TileColor
  = Black
  | White
  deriving (Eq, Show)

type Floor = Map HexCoordinate TileColor

initialFloor :: Floor
initialFloor = M.empty

blackTiles :: Floor -> Int
blackTiles = M.size . M.filter (== Black)

flipTile :: HexCoordinate -> Floor -> Floor
flipTile tile tileFloor =
  case tileFloor !? tile of
    Just Black -> M.insert tile White tileFloor
    _ -> foldr expandFloor (M.insert tile Black tileFloor) $ neighbours tile

expandFloor :: HexCoordinate -> Floor -> Floor
expandFloor tile = M.insertWith (const id) tile White

flipTiles :: [HexCoordinate] -> Floor
flipTiles = foldr flipTile initialFloor

runExhibitDay :: Floor -> Floor
runExhibitDay tileFloor = M.foldrWithKey updateTile tileFloor tileFloor
  where
    blackNeighbours =
      length . filter ((== Just Black) . (tileFloor !?)) . neighbours
    updateTile tile White =
      if blackNeighbours tile == 2
        then flipTile tile
        else id
    updateTile tile Black =
      if blackNeighbours tile == 0 || blackNeighbours tile > 2
        then flipTile tile
        else id

runExhibit :: Int -> Floor -> Floor
runExhibit days = foldr (.) id $ replicate days runExhibitDay
