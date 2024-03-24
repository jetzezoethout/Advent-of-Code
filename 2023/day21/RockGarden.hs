module RockGarden where

import           Coordinate (Coordinate (..))
import           Data.Set   (Set)
import qualified Data.Set   as S
import           Data.Text  (Text)
import           Direction  (allDirections, moveTowards)
import           Grid       (Grid (..), atCoordinate, isInside, parseGrid)

type RockGarden = Grid Bool

parseRockGarden :: Text -> RockGarden
parseRockGarden = parseGrid (== '#')

hasGardenPlotAt :: RockGarden -> Coordinate -> Bool
hasGardenPlotAt garden coord =
  coord `isInside` garden && not (garden `atCoordinate` coord)

freeNeighbours :: RockGarden -> Coordinate -> [Coordinate]
freeNeighbours garden coord =
  filter (garden `hasGardenPlotAt`) $ map (coord `moveTowards`) allDirections

expandTwoSteps ::
     RockGarden
  -> (Set Coordinate, Set Coordinate)
  -> (Set Coordinate, Set Coordinate)
expandTwoSteps garden (reached, justReached) =
  let newReached =
        S.filter (`S.notMember` reached)
          $ S.fromList
              (S.elems justReached
                 >>= freeNeighbours garden
                 >>= freeNeighbours garden)
   in (reached `S.union` newReached, newReached)

reachableAfterSteps :: RockGarden -> Coordinate -> Int -> Int
reachableAfterSteps garden start steps = go (initial, initial) (steps `quot` 2)
  where
    initial =
      if even steps
        then S.singleton start
        else S.fromList $ freeNeighbours garden start
    go reachedSoFar doubleStepsLeft =
      if doubleStepsLeft == 0
        then S.size $ fst reachedSoFar
        else go (expandTwoSteps garden reachedSoFar) (doubleStepsLeft - 1)
