module Hike where

import           Control.Monad.State (State, evalState, gets, modify)
import           Coordinate          (Coordinate)
import           Data.Foldable       (traverse_)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Grid                (atCoordinate)
import           HeightMap           (HeightMap, possibleOrigins, top)

type VisitedLocation = Set Coordinate

type Path = [Coordinate]

descend :: HeightMap -> (Int -> Bool) -> Int
descend heightMap isTarget = evalState (go 0 [start]) $ S.singleton start
  where
    start = top heightMap
    go :: Int -> [Coordinate] -> State VisitedLocation Int
    go _ [] = error "there should be a path"
    go travelled heads =
      if any (isTarget . (heightMap `atCoordinate`)) heads
        then return travelled
        else extend heads >>= go (travelled + 1)
    extend :: [Coordinate] -> State VisitedLocation [Coordinate]
    extend [] = return []
    extend (nextHead:otherHeads) = do
      targets <-
        gets $ \visited ->
          filter (`S.notMember` visited) $ possibleOrigins heightMap nextHead
      traverse_ (modify . S.insert) targets
      otherTargets <- extend otherHeads
      return $ targets <> otherTargets
