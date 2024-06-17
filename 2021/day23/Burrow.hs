module Burrow where

import           Data.PSQueue (Binding ((:->)), PSQ)
import qualified Data.PSQueue as Q
import           Data.Set     (Set)
import qualified Data.Set     as S

data Move a = Move
  { cost   :: Int
  , target :: a
  } deriving (Functor)

class Ord a =>
      Burrow a
  where
  goal :: a
  possibleMoves :: a -> [Move a]
  heuristic :: a -> Int

findOptimalSolution :: Burrow a => a -> Int
findOptimalSolution startBurrow =
  go S.empty $ Q.singleton startBurrow (heuristic startBurrow)
  where
    updateDistance :: Int -> Maybe Int -> Maybe Int
    updateDistance tentativeDistance (Just distance) =
      Just $ min tentativeDistance distance
    updateDistance tentativeDistance Nothing = Just tentativeDistance
    go :: Burrow a => Set a -> PSQ a Int -> Int
    go seen queue =
      case Q.minView queue of
        Nothing -> error "goal cannot be reached"
        Just (nextNode :-> minHeuristic, remainingQueue) ->
          if nextNode == goal
            then minHeuristic
            else let realDistance = minHeuristic - heuristic nextNode
                     neighbors = possibleMoves nextNode
                     toUpdate = filter ((`S.notMember` seen) . target) neighbors
                  in go (S.insert nextNode seen)
                       $ foldr
                           (\Move {..} ->
                              Q.alter
                                (updateDistance
                                   $ realDistance + cost + heuristic target)
                                target)
                           remainingQueue
                           toUpdate
