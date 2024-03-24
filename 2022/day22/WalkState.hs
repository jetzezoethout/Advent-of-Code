module WalkState where

import           Coordinate (Coordinate (..))
import           Data.List  (foldl')
import           Direction
import           Movement   (Movement (..), turn)

data WalkState a = WalkState
  { position :: Coordinate
  , facing   :: Direction
  } deriving (Show)

type ForwardSpec a = WalkState a -> Maybe (WalkState a)

password :: WalkState a -> Int
password WalkState {..} =
  1000 * (position.row + 1) + 4 * (position.column + 1) + directionContribution
  where
    directionContribution =
      case facing of
        East  -> 0
        South -> 1
        West  -> 2
        North -> 3

walk :: ForwardSpec a -> WalkState a -> [Movement] -> WalkState a
walk board = foldl' (walkOneMovement board)

walkOneMovement :: ForwardSpec a -> WalkState a -> Movement -> WalkState a
walkOneMovement _ WalkState {..} (Turn orientation) =
  WalkState {position = position, facing = facing `turn` orientation}
walkOneMovement board state (Forward amount) = go amount state
  where
    go 0 current = current
    go leftToWalk current =
      case board current of
        Nothing   -> current
        Just next -> go (leftToWalk - 1) next

lookingAt :: WalkState a -> Coordinate
lookingAt WalkState {..} = position `moveTowards` facing

stepAndRotateCounterclockwise :: WalkState a -> WalkState a
stepAndRotateCounterclockwise walkState =
  WalkState
    { position = lookingAt walkState
    , facing = counterClockWise $ facing walkState
    }

opposite :: WalkState a -> WalkState a
opposite walkState =
  WalkState {position = lookingAt walkState, facing = invert $ facing walkState}
