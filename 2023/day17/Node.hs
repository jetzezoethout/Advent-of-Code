module Node where

import           Coordinate (Coordinate (..))
import           Direction  (Direction (..), moveTowards)

data Orientation
  = Horizontal
  | Vertical
  deriving (Eq, Ord, Show)

swap :: Orientation -> Orientation
swap Horizontal = Vertical
swap Vertical   = Horizontal

orthogonalDirections :: Orientation -> [Direction]
orthogonalDirections Horizontal = [North, South]
orthogonalDirections Vertical   = [East, West]

data Node = Node
  { coordinate :: Coordinate
  , moving     :: Orientation
  } deriving (Eq, Ord, Show)

targetLocations :: Int -> Node -> [[Node]]
targetLocations maximumMoves Node {..} =
  map
    (\dir ->
       map (flip Node $ swap moving)
         $ take maximumMoves
         $ tail
         $ iterate (`moveTowards` dir) coordinate)
    $ orthogonalDirections moving
