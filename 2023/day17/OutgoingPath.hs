module OutgoingPath where

import           City         (City)
import           Coordinate   (Coordinate (Coordinate))
import           Data.PSQueue (Binding ((:->)), PSQ)
import qualified Data.PSQueue as Q
import           Data.Set     (Set)
import qualified Data.Set     as S
import           Grid         (atCoordinate, isInside)
import           Node         (Node (Node, coordinate), Orientation (..),
                               targetLocations)

data OutgoingPath = OutgoingPath
  { cost   :: Int
  , target :: Node
  } deriving (Show)

processTargetSequence :: City -> [Node] -> [OutgoingPath]
processTargetSequence city nodes =
  zipWith
    OutgoingPath
    (scanl1 (+)
       $ map (city `atCoordinate`)
       $ filter (`isInside` city)
       $ map coordinate nodes)
    nodes

getPathsIn :: City -> Node -> [OutgoingPath]
getPathsIn city node = targetLocations 3 node >>= processTargetSequence city

getUltraPathsIn :: City -> Node -> [OutgoingPath]
getUltraPathsIn city node =
  targetLocations 10 node >>= drop 3 . processTargetSequence city

findShortestPath :: (Node -> [OutgoingPath]) -> (Node -> Bool) -> Int
findShortestPath getOutgoingPaths isFinalNode =
  go S.empty
    $ Q.fromList
        [ Node (Coordinate 0 0) orientation :-> 0
        | orientation <- [Horizontal, Vertical]
        ]
  where
    updateDistance :: Int -> Maybe Int -> Maybe Int
    updateDistance tentativeDistance (Just distance) =
      Just $ min tentativeDistance distance
    updateDistance tentativeDistance Nothing = Just tentativeDistance
    go :: Set Node -> PSQ Node Int -> Int
    go seen queue =
      case Q.minView queue of
        Nothing -> error "goal cannot be reached"
        Just (nextNode :-> minDistance, remainingQueue) ->
          if isFinalNode nextNode
            then minDistance
            else let toUpdate =
                       filter ((`S.notMember` seen) . target)
                         $ getOutgoingPaths nextNode
                  in go (S.insert nextNode seen)
                       $ foldr
                           (\OutgoingPath {..} ->
                              Q.alter
                                (updateDistance $ minDistance + cost)
                                target)
                           remainingQueue
                           toUpdate
