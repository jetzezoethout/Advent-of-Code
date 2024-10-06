module HexCoordinate where

import           Data.List    (foldl')
import           HexDirection (HexDirection (..), allHexDirections)

data HexCoordinate = HexCoordinate
  { x :: Int
  , y :: Int
  , z :: Int
  } deriving (Show)

instance Eq HexCoordinate where
  (==) :: HexCoordinate -> HexCoordinate -> Bool
  c1 == c2 = c1.x == c2.x && c1.y == c2.y

instance Ord HexCoordinate where
  compare :: HexCoordinate -> HexCoordinate -> Ordering
  c1 `compare` c2 = c1.x `compare` c2.x <> c1.y `compare` c2.y

referenceTile :: HexCoordinate
referenceTile = HexCoordinate {x = 0, y = 0, z = 0}

moveTowards :: HexCoordinate -> HexDirection -> HexCoordinate
HexCoordinate {..} `moveTowards` East =
  HexCoordinate {x = x, y = y + 1, z = z - 1}
HexCoordinate {..} `moveTowards` NorthEast =
  HexCoordinate {x = x - 1, y = y + 1, z = z}
HexCoordinate {..} `moveTowards` NorthWest =
  HexCoordinate {x = x - 1, y = y, z = z + 1}
HexCoordinate {..} `moveTowards` West =
  HexCoordinate {x = x, y = y - 1, z = z + 1}
HexCoordinate {..} `moveTowards` SouthWest =
  HexCoordinate {x = x + 1, y = y - 1, z = z}
HexCoordinate {..} `moveTowards` SouthEast =
  HexCoordinate {x = x + 1, y = y, z = z - 1}

walk :: [HexDirection] -> HexCoordinate
walk = foldl' moveTowards referenceTile

neighbours :: HexCoordinate -> [HexCoordinate]
neighbours tile = map (tile `moveTowards`) allHexDirections
