module TargetArea where

import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseInt)

data TargetArea = TargetArea
  { xMin :: Int
  , xMax :: Int
  , yMin :: Int
  , yMax :: Int
  }

parseTargetArea :: Text -> TargetArea
parseTargetArea text =
  let parts = T.splitOn ", y=" $ T.drop 15 text
      xBounds = T.splitOn ".." $ head parts
      yBounds = T.splitOn ".." $ parts !! 1
   in TargetArea
        { xMin = parseInt $ head xBounds
        , xMax = parseInt $ xBounds !! 1
        , yMin = parseInt $ head yBounds
        , yMax = parseInt $ yBounds !! 1
        }

minXVel :: TargetArea -> Int
minXVel TargetArea {..} = head $ filter triangularInside [1 ..]
  where
    triangularInside x = x * (x + 1) >= 2 * xMin

maxXVel :: TargetArea -> Int
maxXVel = xMax

minYVel :: TargetArea -> Int
minYVel = yMin

maxYVel :: TargetArea -> Int
maxYVel TargetArea {..} = -1 - yMin

highestY :: TargetArea -> Int
highestY targetArea = (maxYVel targetArea * (maxYVel targetArea + 1)) `div` 2

isInside :: TargetArea -> Int -> Int -> Bool
isInside TargetArea {..} x y = xMin <= x && x <= xMax && yMin <= y && y <= yMax

hasMissed :: TargetArea -> Int -> Int -> Bool
hasMissed TargetArea {..} x y = x > xMax || y < yMin
