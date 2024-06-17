module Point where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)
import           Vector

newtype Point =
  Point [Int]
  deriving (Eq, Ord, Show)

parsePoint :: Text -> Point
parsePoint = Point . map parseInt . T.splitOn ","

origin :: Point
origin = Point [0, 0, 0]

to :: Point -> Point -> Vector
(Point coordinates1) `to` (Point coordinates2) =
  Vector $ zipWith (-) coordinates2 coordinates1

along :: Point -> Vector -> Point
(Point coordinates1) `along` (Vector coordinates2) =
  Point $ zipWith (+) coordinates1 coordinates2

allPairs :: [Point] -> [(Point, Point)]
allPairs = go
  where
    go []     = []
    go (p:ps) = map (p, ) ps <> go ps

manhattanDistance :: Point -> Point -> Int
manhattanDistance p1 p2 = norm $ p1 `to` p2

manhattanDiameter :: [Point] -> Int
manhattanDiameter = maximum . map (uncurry manhattanDistance) . allPairs

data CachedNormalVector = CachedNormalVector
  { p1           :: Point
  , p2           :: Point
  , normalVector :: Vector
  }

cacheNormalVector :: Point -> Point -> CachedNormalVector
cacheNormalVector p1 p2 =
  CachedNormalVector {p1 = p1, p2 = p2, normalVector = normalize $ p1 `to` p2}

allNormalVectors :: [Point] -> [CachedNormalVector]
allNormalVectors points =
  [cacheNormalVector p1 p2 | (p1, p2) <- allPairs points]
