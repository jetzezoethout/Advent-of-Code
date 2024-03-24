module StormJourney where

import           Control.Monad.State (State, evalState, gets, modify)
import           Coordinate          (Coordinate (..))
import           Data.Sequence       (Seq (..), ViewL (..), fromList, singleton,
                                      viewl, (><))
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Direction           (allDirections, moveTowards)
import           Storm               (Storm (..), isFreeAtStage)

data Node = Node
  { position   :: Coordinate
  , cycleStage :: Int
  } deriving (Show, Eq, Ord)

type VisitedNodes = Set Node

data SearchStage = SearchStage
  { node     :: Node
  , distance :: Int
  } deriving (Show)

shortestPath :: Storm -> Int -> Coordinate -> Coordinate -> Int
shortestPath storm@Storm {..} afterMinutes start finish =
  evalState (go $ singleton $ SearchStage startingNode 0)
    $ S.singleton startingNode
  where
    startingNode = Node start (afterMinutes `mod` cycleLength)
    isFinalNode Node {..} = position == finish
    neighbours Node {..} = map (`Node` nextStage) allowed
      where
        nextStage = (cycleStage + 1) `mod` cycleLength
        possibilities = position : map (position `moveTowards`) allDirections
        allowed = filter (isFreeAtStage storm nextStage) possibilities
    go :: Seq SearchStage -> State VisitedNodes Int
    go nodesToCheck =
      case viewl nodesToCheck of
        EmptyL -> error "goal unreachable"
        SearchStage {..} :< otherNodes -> do
          targets <-
            gets $ \visited -> filter (`S.notMember` visited) $ neighbours node
          if any isFinalNode targets
            then return $ distance + 1
            else mapM_ (modify . S.insert) targets
                   >> go
                        (otherNodes
                           >< fromList
                                (map (`SearchStage` (distance + 1)) targets))
