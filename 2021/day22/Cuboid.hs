module Cuboid where

import           Data.Text (Text)
import qualified Data.Text as T
import           Range     (Range, intersectRanges, parseRange, size)

data Cuboid = Cuboid
  { xRange :: Range
  , yRange :: Range
  , zRange :: Range
  } deriving (Show)

parseCuboid :: Text -> Cuboid
parseCuboid text =
  let parts = T.splitOn "," text
   in Cuboid
        { xRange = parseRange $ T.drop 2 $ head parts
        , yRange = parseRange $ T.drop 2 $ parts !! 1
        , zRange = parseRange $ T.drop 2 $ parts !! 2
        }

intersect :: Cuboid -> Cuboid -> Maybe Cuboid
intersect c1 c2 = do
  xIntersect <- intersectRanges c1.xRange c2.xRange
  yIntersect <- intersectRanges c1.yRange c2.yRange
  zIntersect <- intersectRanges c1.zRange c2.zRange
  return $ Cuboid xIntersect yIntersect zIntersect

cubes :: Cuboid -> Int
cubes Cuboid {..} = size xRange * size yRange * size zRange
