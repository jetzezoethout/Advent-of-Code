module Coordinate3D where

import           Spatial (Spatial (..))

data Coordinate3D = Coordinate3D
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Eq, Ord, Show)

instance Spatial Coordinate3D where
  neighbours :: Coordinate3D -> [Coordinate3D]
  neighbours Coordinate3D {..} =
    [ Coordinate3D (x + dx) (y + dy) (z + dz)
    | dx <- [-1, 0, 1]
    , dy <- [-1, 0, 1]
    , dz <- [-1, 0, 1]
    , not (dx == 0 && dy == 0 && dz == 0)
    ]
