module Image where

import           Algorithm   (Algorithm, darkVoid, decode, lightVoid)
import           Coordinate  (Coordinate (Coordinate), addCoordinate)
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, generate)
import qualified Data.Vector as V
import           Grid        (Grid (..), atCoordinate, fromNestedList, isInside)
import           Pixel       (Pixel (..), fromChar)

data Image = Image
  { outer :: Pixel
  , inner :: Grid Pixel
  } deriving (Show)

parseImage :: [Text] -> Image
parseImage textLines =
  Image
    { outer = Dark
    , inner = fromNestedList $ map (map fromChar . T.unpack) textLines
    }

lightPixels :: Image -> Int
lightPixels Image {..} =
  case outer of
    Dark  -> V.sum $ V.map (V.length . V.filter (== Light)) inner.grid
    Light -> error "infinitely many pixels are lit"

neighbours :: Coordinate -> [Coordinate]
neighbours coord =
  [coord `addCoordinate` Coordinate dr dc | dr <- [-1 .. 1], dc <- [-1 .. 1]]

atShiftedCoordinate :: Image -> Coordinate -> Pixel
Image {..} `atShiftedCoordinate` coord =
  let shiftedCoord = coord `addCoordinate` Coordinate (-1) (-1)
   in if shiftedCoord `isInside` inner
        then inner `atCoordinate` shiftedCoord
        else outer

enhance :: Algorithm -> Image -> Image
enhance algorithm image =
  Image
    { outer =
        case image.outer of
          Dark  -> darkVoid algorithm
          Light -> lightVoid algorithm
    , inner =
        Grid
          { height = newHeight
          , width = newWidth
          , grid = generate newHeight generateRow
          }
    }
  where
    newHeight = image.inner.height + 2
    newWidth = image.inner.width + 2
    generateRow :: Int -> Vector Pixel
    generateRow rowIndex =
      generate newWidth $ \colIndex ->
        decode algorithm
          $ map (image `atShiftedCoordinate`)
          $ neighbours
          $ Coordinate rowIndex colIndex
