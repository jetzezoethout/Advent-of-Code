module Puzzle where

import           Control.Monad.Reader (ReaderT (..), asks)
import           Control.Monad.State  (State, execState, gets, modify)
import           Coordinate           (Coordinate (..), dilate)
import           Data.Foldable        (traverse_)
import           Data.Map             (Map, (!), (!?))
import qualified Data.Map             as M
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Direction            (Direction (..), moveTowards)
import           LinkedTile           (LinkedTile (links, tile), isCornerTile)
import           Reflectable          (Reflectable (reflectRows),
                                       reflectColumns)
import           Rotatable            (rotateUntil)
import           Tile                 (TileID, move)

type Puzzle = Set Coordinate

type PuzzleTable = Map Coordinate (TileID, LinkedTile)

type PuzzleContext = ReaderT (Map TileID LinkedTile) (State PuzzleTable)

puzzleSize :: PuzzleContext Int
puzzleSize = round . sqrt <$> squaredSize
  where
    squaredSize :: PuzzleContext Double
    squaredSize = asks $ fromIntegral . M.size

layCornerTile :: PuzzleContext ()
layCornerTile = do
  (tileId, cornerTile) <- asks $ head . M.toList . M.filter isCornerTile
  let isRotatedCorrectly linkedTile =
        (East `M.member` links linkedTile)
          && (South `M.member` links linkedTile)
      correctlyRotatedTile = rotateUntil isRotatedCorrectly cornerTile
  modify $ M.insert (Coordinate 0 0) (tileId, correctlyRotatedTile)

extendRow :: Coordinate -> PuzzleContext ()
extendRow toExtend = do
  (baseTileId, baseTile) <- gets (! (toExtend `moveTowards` West))
  let fittingTileId = baseTile.links ! East
  fittingTile <- asks (! fittingTileId)
  let isRotatedCorrectly linkedTile =
        links linkedTile !? West == Just baseTileId
      correctlyRotatedTile = rotateUntil isRotatedCorrectly fittingTile
  laidNorth <- gets (!? (toExtend `moveTowards` North))
  let isOrientedCorrectly =
        (fst <$> laidNorth) == links correctlyRotatedTile !? North
      correctlyOrientedTile =
        if isOrientedCorrectly
          then correctlyRotatedTile
          else reflectRows correctlyRotatedTile
  modify $ M.insert toExtend (fittingTileId, correctlyOrientedTile)

completeRow :: Int -> PuzzleContext ()
completeRow rowIndex = do
  size <- puzzleSize
  traverse_ (extendRow . Coordinate rowIndex) [1 .. size - 1]

layTopRow :: PuzzleContext ()
layTopRow = layCornerTile >> completeRow 0

startNewRow :: Int -> PuzzleContext ()
startNewRow rowIndex = do
  let toExtend = Coordinate rowIndex 0
  (baseTileId, baseTile) <- gets (! (toExtend `moveTowards` North))
  let fittingTileId = baseTile.links ! South
  fittingTile <- asks (! fittingTileId)
  let isRotatedCorrectly linkedTile =
        links linkedTile !? North == Just baseTileId
      correctlyRotatedTile = rotateUntil isRotatedCorrectly fittingTile
      isOrientedCorrectly = East `M.member` correctlyRotatedTile.links
      correctlyOrientedTile =
        if isOrientedCorrectly
          then correctlyRotatedTile
          else reflectColumns correctlyRotatedTile
  modify $ M.insert toExtend (fittingTileId, correctlyOrientedTile)

addRow :: Int -> PuzzleContext ()
addRow rowIndex = startNewRow rowIndex >> completeRow rowIndex

layPuzzle :: PuzzleContext ()
layPuzzle = do
  size <- puzzleSize
  layTopRow
  traverse_ addRow [1 .. size - 1]

constructPuzzle :: Map TileID LinkedTile -> Puzzle
constructPuzzle linkedTiles =
  M.foldl' S.union S.empty $ M.mapWithKey positionCorrectly completedPuzzle
  where
    completedPuzzle = execState (runReaderT layPuzzle linkedTiles) M.empty
    positionCorrectly :: Coordinate -> (TileID, LinkedTile) -> Set Coordinate
    positionCorrectly placeOnTable (_, linkedTile) =
      move (10 `dilate` placeOnTable) linkedTile.tile

removeEdges :: Puzzle -> Puzzle
removeEdges = S.foldl' build S.empty
  where
    adjustIndex :: Int -> Maybe Int
    adjustIndex x
      | x `mod` 10 == 0 || x `mod` 10 == 9 = Nothing
      | otherwise = Just $ 8 * (x `div` 10) + x `mod` 10 - 1
    adjustPosition :: Coordinate -> Maybe Coordinate
    adjustPosition Coordinate {..} = do
      newRow <- adjustIndex row
      newColumn <- adjustIndex column
      return $ Coordinate {row = newRow, column = newColumn}
    build :: Puzzle -> Coordinate -> Puzzle
    build acc coord =
      case adjustPosition coord of
        Nothing          -> acc
        Just newPosition -> S.insert newPosition acc
