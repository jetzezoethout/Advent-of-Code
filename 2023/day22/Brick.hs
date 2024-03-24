module Brick where

import           Coordinate    (Coordinate (..))
import           Data.Function (on)
import           Data.List     (sortBy)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Parsers       (parseUnsignedInt)

data Brick
  = XBrick
      { x              :: Int
      , y              :: Int
      , z              :: Int
      , lengthMinusOne :: Int
      }
  | YBrick
      { x              :: Int
      , y              :: Int
      , z              :: Int
      , lengthMinusOne :: Int
      }
  | ZBrick
      { x              :: Int
      , y              :: Int
      , z              :: Int
      , lengthMinusOne :: Int
      }
  deriving (Show)

parseBrick :: Text -> Brick
parseBrick text
  | x1 == x2 && y1 == y2 = ZBrick x1 y1 (min z1 z2) (abs (z1 - z2))
  | x1 == x2 = YBrick x1 (min y1 y2) z1 (abs (y1 - y2))
  | otherwise = XBrick (min x1 x2) y1 z1 (abs (x1 - x2))
  where
    parts = T.split (== '~') text
    firstEnd = map parseUnsignedInt $ T.split (== ',') $ head parts
    secondEnd = map parseUnsignedInt $ T.split (== ',') $ parts !! 1
    (x1, y1, z1) = (head firstEnd, firstEnd !! 1, firstEnd !! 2)
    (x2, y2, z2) = (head secondEnd, secondEnd !! 1, secondEnd !! 2)

groundShadow :: Brick -> [Coordinate]
groundShadow XBrick {..} = [Coordinate x' y | x' <- [x .. x + lengthMinusOne]]
groundShadow YBrick {..} = [Coordinate x y' | y' <- [y .. y + lengthMinusOne]]
groundShadow ZBrick {..} = [Coordinate x y]

sortByZ :: [Brick] -> [Brick]
sortByZ = sortBy (compare `on` z)

highestZ :: Brick -> Int
highestZ ZBrick {..} = z + lengthMinusOne
highestZ brick       = z brick

dropToZ :: Int -> Brick -> Brick
dropToZ targetZ brick = brick {z = targetZ}
