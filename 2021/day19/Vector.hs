module Vector where

import           Data.List (sort)

newtype Vector =
  Vector [Int]
  deriving (Eq, Ord, Show)

norm :: Vector -> Int
norm (Vector coordinates) = sum $ map abs coordinates

normalize :: Vector -> Vector
normalize (Vector coordinates) = Vector $ sort $ map abs coordinates

findRotation :: Vector -> Vector -> Maybe LinearTransformation
findRotation v1 v2 =
  case filter (\f -> apply f v1 == v2) rotations of
    []         -> Nothing
    rotation:_ -> Just rotation

newtype LinearTransformation =
  LinearTransformation [[Int]]

apply :: LinearTransformation -> Vector -> Vector
apply (LinearTransformation matrix) (Vector coordinates) =
  Vector $ map (`innerProduct` coordinates) matrix
  where
    coordinates1 `innerProduct` coordinates2 =
      sum $ zipWith (*) coordinates1 coordinates2

rotations :: [LinearTransformation]
rotations =
  map LinearTransformation $ do
    a <- [-1, 1]
    b <- [-1, 1]
    matrices a b
  where
    matrices a b =
      [ [[a, 0, 0], [0, b, 0], [0, 0, a * b]]
      , [[a, 0, 0], [0, 0, b], [0, -(a * b), 0]]
      , [[0, a, 0], [0, 0, b], [a * b, 0, 0]]
      , [[0, a, 0], [b, 0, 0], [0, 0, -(a * b)]]
      , [[0, 0, a], [b, 0, 0], [0, a * b, 0]]
      , [[0, 0, a], [0, b, 0], [-(a * b), 0, 0]]
      ]
