module Hike where

import           Control.Monad   (guard)
import           Data.IntSet     (IntSet)
import qualified Data.IntSet     as S
import           Data.Map.Strict ((!))
import           Data.Maybe      (mapMaybe)
import           Graph           (Graph, NodeId, OutgoingPath (..),
                                  penultimateStep)

type VisitedNodes = Int

data Hike = Hike
  { visited       :: IntSet
  , currentlyAt   :: NodeId
  , totalDistance :: Int
  }

initialHike :: Int -> Hike
initialHike nodeId =
  Hike {visited = S.singleton nodeId, currentlyAt = nodeId, totalDistance = 0}

takePath :: Hike -> OutgoingPath -> Maybe Hike
takePath Hike {..} OutgoingPath {..} =
  guard (not (target `S.member` visited))
    >> Just
         (Hike
            { visited = S.insert target visited
            , currentlyAt = target
            , totalDistance = totalDistance + distance
            })

findLongestPath :: Graph -> Int
findLongestPath graph = finalDistance + go 0 [initialHike 0]
  where
    (realFinish, finalDistance) = penultimateStep graph
    go :: Int -> [Hike] -> Int
    go maxSoFar hikeQueue =
      case hikeQueue of
        [] -> maxSoFar
        hike@Hike {..}:remaingHikes ->
          if currentlyAt == realFinish
            then go (max maxSoFar totalDistance) remaingHikes
            else go maxSoFar $ proceed hike <> remaingHikes
    proceed :: Hike -> [Hike]
    proceed hike@Hike {..} = mapMaybe (takePath hike) $ graph ! currentlyAt
