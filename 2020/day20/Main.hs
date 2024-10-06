module Main where

import qualified Data.Map    as M
import           LinkedTile  (isCornerTile, linkTiles)
import           ProcessFile (processFile)
import           Puzzle      (constructPuzzle, removeEdges)
import           SeaMonster  (monsterFree)
import           Tile        (parseTiles)

main :: IO ()
main =
  processFile $ \text -> do
    let tiles = parseTiles text
        linkedTiles = linkTiles tiles
        cornerTiles = M.filter isCornerTile linkedTiles
        puzzle = removeEdges $ constructPuzzle linkedTiles
    print $ product $ M.keys cornerTiles
    print $ monsterFree puzzle
