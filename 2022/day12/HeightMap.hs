module HeightMap where

import           Control.Monad (guard)
import           Coordinate    (Coordinate)
import           Data.Maybe    (fromJust, mapMaybe)
import           Data.Text     (Text)
import           Direction     (allDirections, moveTowards)
import           Grid          (Grid, atCoordinate, findInGrid, parseGrid,
                                safeAtCoordinate)

type HeightMap = Grid Int

parseHeightMap :: Text -> Grid Int
parseHeightMap = parseGrid fromChar

fromChar :: Char -> Int
fromChar 'S'       = 0
fromChar 'E'       = 27
fromChar lowerCase = fromEnum lowerCase - 96

top :: HeightMap -> Coordinate
top = fromJust . findInGrid (== 27)

possibleOrigins :: HeightMap -> Coordinate -> [Coordinate]
possibleOrigins heightMap position =
  mapMaybe (getPossibleOrigin . (position `moveTowards`)) allDirections
  where
    ourHeight = heightMap `atCoordinate` position
    getPossibleOrigin coordinate = do
      originHeight <- heightMap `safeAtCoordinate` coordinate
      guard $ ourHeight - originHeight <= 1
      return coordinate
