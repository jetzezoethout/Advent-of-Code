module HeightMap where

import           Coordinate (Coordinate (..))
import qualified Data.Set   as S
import           Data.Text  (Text)
import           Direction
import           Grid       (Grid (..), atCoordinate, parseGrid,
                             safeAtCoordinate)

type HeightMap = Grid Int

parseHeightMap :: Text -> Grid Int
parseHeightMap = parseGrid parseHeight
  where
    parseHeight ch = fromEnum ch - fromEnum '0'

isLowPoint :: HeightMap -> Coordinate -> Bool
isLowPoint heightMap coord = all isHigherTowards allDirections
  where
    localHeight = heightMap `atCoordinate` coord
    isHigherTowards dir =
      maybe
        True
        (> localHeight)
        (heightMap `safeAtCoordinate` (coord `moveTowards` dir))

lowPoints :: HeightMap -> [Coordinate]
lowPoints heightMap =
  filter
    (isLowPoint heightMap)
    [ Coordinate r c
    | r <- [0 .. heightMap.height - 1]
    , c <- [0 .. heightMap.width - 1]
    ]

basinSize :: HeightMap -> Coordinate -> Int
basinSize heightMap coord = go [coord] $ S.singleton coord
  where
    go [] seen = S.size seen
    go (next:remainder) seen =
      let newFound =
            filter
              (\neighbour -> isInBasin neighbour && neighbour `S.notMember` seen)
              $ map (next `moveTowards`) allDirections
       in go (newFound <> remainder) $ seen `S.union` S.fromList newFound
    isInBasin candidate =
      maybe False (< 9) $ heightMap `safeAtCoordinate` candidate
