module Coordinate4D where

import           Spatial (Spatial (..))

data Coordinate4D = Coordinate4D
  { x :: Int
  , y :: Int
  , z :: Int
  , w :: Int
  } deriving (Eq, Ord, Show)

instance Spatial Coordinate4D where
  neighbours :: Coordinate4D -> [Coordinate4D]
  neighbours Coordinate4D {..} =
    [ Coordinate4D (x + dx) (y + dy) (z + dz) (w + dw)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , dw <- [-1, 0, 1]
    , not (dx == 0 && dy == 0 && dz == 0 && dw == 0)
    ]
