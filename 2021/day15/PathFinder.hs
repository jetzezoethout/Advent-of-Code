module PathFinder where

import           Coordinate   (Coordinate (..))
import           Data.PSQueue (Binding ((:->)), PSQ)
import qualified Data.PSQueue as Q
import           Data.Set     (Set)
import qualified Data.Set     as S

data SearchSpace = SearchSpace
  { neighbours :: Coordinate -> [Coordinate]
  , cost       :: Coordinate -> Int
  , isFinish   :: Coordinate -> Bool
  }

findShortestPath :: SearchSpace -> Int
findShortestPath SearchSpace {..} = go S.empty $ Q.singleton (Coordinate 0 0) 0
  where
    updateDistance :: Int -> Maybe Int -> Maybe Int
    updateDistance tentativeDistance (Just distance) =
      Just $ min tentativeDistance distance
    updateDistance tentativeDistance Nothing = Just tentativeDistance
    go :: Set Coordinate -> PSQ Coordinate Int -> Int
    go seen queue =
      case Q.minView queue of
        Nothing -> error "goal cannot be reached"
        Just (nextNode :-> minDistance, remainingQueue) ->
          if isFinish nextNode
            then minDistance
            else let toUpdate =
                       filter (`S.notMember` seen) $ neighbours nextNode
                  in go (S.insert nextNode seen)
                       $ foldr
                           (\neighbour ->
                              Q.alter
                                (updateDistance $ minDistance + cost neighbour)
                                neighbour)
                           remainingQueue
                           toUpdate
