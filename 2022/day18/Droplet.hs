module Droplet where

import           Coordinate3D (Coordinate3D (..), neighbours, parseCoordinate3D)
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)
import qualified Data.Text    as T

type Droplet = Set Coordinate3D

parseDroplet :: Text -> Droplet
parseDroplet = S.fromList . map parseCoordinate3D . T.lines

surfaceArea :: Droplet -> Int
surfaceArea droplet = sum $ map exposedSides $ S.toList droplet
  where
    exposedSides :: Coordinate3D -> Int
    exposedSides = length . filter (`S.notMember` droplet) . neighbours

corners :: Droplet -> (Coordinate3D, Coordinate3D)
corners droplet =
  let xRange = S.map x droplet
      yRange = S.map y droplet
      zRange = S.map z droplet
   in ( Coordinate3D
          (S.findMin xRange - 1)
          (S.findMin yRange - 1)
          (S.findMin zRange - 1)
      , Coordinate3D
          (S.findMax xRange + 1)
          (S.findMax yRange + 1)
          (S.findMax zRange + 1))

exteriorSurfaceArea :: Droplet -> Int
exteriorSurfaceArea droplet = go (S.singleton minCorner) [minCorner] 0
  where
    (minCorner, maxCorner) = corners droplet
    isInBox :: Coordinate3D -> Bool
    isInBox Coordinate3D {..} =
      minCorner.x <= x
        && x <= maxCorner.x
        && minCorner.y <= y
        && y <= maxCorner.y
        && minCorner.z <= z
        && z <= maxCorner.z
    go :: Set Coordinate3D -> [Coordinate3D] -> Int -> Int
    go _ [] areaFound = areaFound
    go seen (next:remaining) areaFound =
      let newCoordinates =
            filter (\c -> isInBox c && c `S.notMember` seen) $ neighbours next
          newFloodFill = filter (`S.notMember` droplet) newCoordinates
          newArea = length newCoordinates - length newFloodFill
       in go
            (seen `S.union` S.fromList newFloodFill)
            (newFloodFill <> remaining)
            (areaFound + newArea)
