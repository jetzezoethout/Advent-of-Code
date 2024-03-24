module Faces where

import           Board               (Board, UnitSquare, isOnBoard, sideLength)
import           Control.Monad.State (State, evalState, gets, modify)
import           Coordinate          (Coordinate (..), addCoordinates)
import           Direction           (Direction (East, North, South, West))
import           Grid                (atCoordinate)
import           Tile                (Tile (..))
import           WalkState           (WalkState (..), lookingAt, opposite,
                                      stepAndRotateCounterclockwise)

data CubeFace

traverseCorner :: Board -> State (WalkState CubeFace) Int
traverseCorner board = modify opposite >> go 0
  where
    go :: Int -> State (WalkState CubeFace) Int
    go turns = do
      oneAhead <- gets lookingAt
      if zoomInToCorner board oneAhead `isOnBoard` board
        then modify stepAndRotateCounterclockwise >> go (turns + 1)
        else return turns

findEntrance :: Board -> WalkState CubeFace -> WalkState CubeFace
findEntrance board = evalState $ traverseCorner board >>= go 0
  where
    go :: Int -> Int -> State (WalkState CubeFace) (WalkState CubeFace)
    go evenCorners oddCorners =
      if evenCorners `mod` 3 == 0 && oddCorners `mod` 3 == 0
        then gets opposite
        else do
          evenCorner <- traverseCorner board
          oddCorner <- traverseCorner board
          go (evenCorners + evenCorner) (oddCorners + oddCorner)

moveWithCube :: Board -> WalkState UnitSquare -> Maybe (WalkState UnitSquare)
moveWithCube board start =
  let canGoForward = lookingAt start `isOnBoard` board
      adjustedStart =
        if canGoForward
          then start
          else let (facingOut, offset) = zoomOut board start
                   facingIn = findEntrance board facingOut
                in zoomIn board facingIn offset
   in case board `atCoordinate` lookingAt adjustedStart of
        Open -> Just $ WalkState (lookingAt adjustedStart) adjustedStart.facing
        Wall -> Nothing
        Void -> error "This isn't supposed to happen"

zoomOut :: Board -> WalkState UnitSquare -> (WalkState CubeFace, Int)
zoomOut board WalkState {..} =
  let squaresToTheLeft =
        case facing of
          North -> position.column
          East  -> position.row
          South -> sideLength board - position.column - 1
          West  -> sideLength board - position.row - 1
      faceCoordinate =
        Coordinate
          (position.row `div` sideLength board)
          (position.column `div` sideLength board)
   in (WalkState faceCoordinate facing, squaresToTheLeft `mod` sideLength board)

zoomInToCorner :: Board -> Coordinate -> Coordinate
zoomInToCorner board Coordinate {..} =
  Coordinate (row * sideLength board) (column * sideLength board)

zoomIn :: Board -> WalkState CubeFace -> Int -> WalkState UnitSquare
zoomIn board WalkState {..} offset =
  let side = sideLength board
      offSetInFace =
        case facing of
          North -> Coordinate 0 offset
          East  -> Coordinate offset (side - 1)
          South -> Coordinate (side - 1) (side - 1 - offset)
          West  -> Coordinate (side - 1 - offset) 0
   in WalkState
        (zoomInToCorner board position `addCoordinates` offSetInFace)
        facing
