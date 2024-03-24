module HailStone where

import           Control.Monad (guard)
import           Data.Maybe    (mapMaybe)
import           Data.Ratio    ((%))
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Vector        (Three, Two, Vector (Vector), add, crossProduct,
                                dropZ, innerProduct, minus, parseVector, scalar,
                                solve2, toFrac, x)

data HailStone size = HailStone
  { position :: Vector size Integer
  , velocity :: Vector size Integer
  } deriving (Show)

parseHailStone :: Text -> HailStone Three
parseHailStone text =
  HailStone
    {position = parseVector $ head parts, velocity = parseVector $ parts !! 1}
  where
    parts = T.splitOn " @ " text

atTime :: HailStone size -> Rational -> Vector size Rational
HailStone {..} `atTime` t = toFrac position `add` (t `scalar` toFrac velocity)

flatten :: HailStone Three -> HailStone Two
flatten HailStone {..} =
  HailStone {position = dropZ position, velocity = dropZ velocity}

intersect :: HailStone Two -> HailStone Two -> Maybe (Vector Two Rational)
h1 `intersect` h2 = do
  coefficients <-
    solve2
      $ Vector
          [h1.velocity, minus h2.velocity, h2.position `add` minus h1.position]
  guard (all (>= 0 % 1) coefficients)
  return $ h1 `atTime` x coefficients

allIntersections :: [HailStone Two] -> [Vector Two Rational]
allIntersections [] = []
allIntersections (hail:hails) =
  mapMaybe (hail `intersect`) hails <> allIntersections hails

inReferenceFrameOf :: HailStone size -> HailStone size -> HailStone size
h1 `inReferenceFrameOf` h2 =
  HailStone
    { position = h1.position `add` minus h2.position
    , velocity = h1.velocity `add` minus h2.velocity
    }

-- Normal vector to the plane through the origin and the HailStone trajectory
planeNormal :: HailStone Three -> Vector Three Integer
planeNormal HailStone {..} = crossProduct position velocity

findRock :: [HailStone Three] -> Vector Three Rational
findRock stones =
  let referenceStone = head stones
      -- View the other two hailstones in the reference frame of the first one
      h1 = stones !! 1 `inReferenceFrameOf` referenceStone
      h2 = stones !! 2 `inReferenceFrameOf` referenceStone
      -- Find the intersection on h1 by intersecting with the plane of h2
      n2 = planeNormal h2
      t1 = -(h1.position `innerProduct` n2) % (h1.velocity `innerProduct` n2)
      intersect1 = h1 `atTime` t1
      -- Similarly for the other intersection
      n1 = planeNormal h1
      t2 = -(h2.position `innerProduct` n1) % (h2.velocity `innerProduct` n1)
      intersect2 = h2 `atTime` t2
      -- Find the original rock position using the time ratios
      rockPosition =
        ((t2 / (t2 - t1)) `scalar` intersect1)
          `add` ((-t1 / (t2 - t1)) `scalar` intersect2)
      -- Finally, transform back to the original frame
   in rockPosition `add` toFrac referenceStone.position
