module ConwayCubes where

import           Coordinate   (Coordinate (..))
import           Coordinate3D (Coordinate3D (..))
import           Coordinate4D (Coordinate4D (Coordinate4D))
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)
import           LocatedChar  (LocatedChar (..), locateText)
import           Spatial      (Spatial (..))

newtype ConwayCubes a = ConwayCubes
  { activeCubes :: Set a
  }

amountActive :: ConwayCubes a -> Int
amountActive ConwayCubes {..} = S.size activeCubes

parseConwayCubesXY :: Text -> [Coordinate]
parseConwayCubesXY = map location . filter ((== '#') . char) . locateText

parseConwayCubes3D :: Text -> ConwayCubes Coordinate3D
parseConwayCubes3D =
  ConwayCubes
    . S.fromList
    . map (\Coordinate {..} -> Coordinate3D row column 0)
    . parseConwayCubesXY

parseConwayCubes4D :: Text -> ConwayCubes Coordinate4D
parseConwayCubes4D =
  ConwayCubes
    . S.fromList
    . map (\Coordinate {..} -> Coordinate4D row column 0 0)
    . parseConwayCubesXY

cycleOnce :: Spatial a => ConwayCubes a -> ConwayCubes a
cycleOnce ConwayCubes {..} = ConwayCubes $ S.filter becomesActive toCheck
  where
    toCheck =
      S.foldl'
        (\acc -> (acc `S.union`) . S.fromList . neighbours)
        S.empty
        activeCubes
    activeNeighbours = length . filter (`S.member` activeCubes) . neighbours
    becomesActive coordinate =
      activeNeighbours coordinate == 3
        || (coordinate `S.member` activeCubes
              && activeNeighbours coordinate == 2)

cycleTimes :: Spatial a => Int -> ConwayCubes a -> ConwayCubes a
cycleTimes n = foldr (.) id $ replicate n cycleOnce
