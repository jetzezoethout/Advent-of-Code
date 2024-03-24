module MinCutFinder where

import           Control.Monad       (guard)
import           Control.Monad.State (State, evalState, gets, modify)
import           Data.IntMap.Strict  ((!))
import           Data.IntSet         (IntSet)
import qualified Data.IntSet         as S
import           Data.List           (find, foldl')
import           Graph               (Graph, Node, deleteEdge, insertEdge)

type Path = [Node]

type BFSResult = Either Path Int

type VisitedNodes = IntSet

-- Return a (reversed) shortest path from source to target, or the size of the
-- connected component of source if such a path does not exist.
shortestPathOrConnectedGroup :: Graph -> Node -> Node -> BFSResult
shortestPathOrConnectedGroup graph source target =
  evalState (go [[source]]) $ S.singleton source
  where
    go :: [Path] -> State VisitedNodes BFSResult
    go [] = gets (Right . S.size)
    go batch = do
      nextBatch <- oneDeeper batch
      case find ((== target) . head) nextBatch of
        Just path -> return $ Left path
        Nothing   -> go nextBatch
    oneDeeper :: [Path] -> State VisitedNodes [Path]
    oneDeeper [] = return []
    oneDeeper (nextPath:paths) = do
      let currentLocation = head nextPath
      targets <-
        gets $ \visited ->
          filter (`S.notMember` visited) (graph ! currentLocation)
      mapM_ (modify . S.insert) targets
      extendOtherPaths <- oneDeeper paths
      return $ map (: nextPath) targets <> extendOtherPaths

-- Find a cut with cost three between source and target if it exists, and if so,
-- return the size of the cutgroup containing source
cutGroupWithCost3 :: Graph -> Node -> Node -> Maybe Int
cutGroupWithCost3 graph source target = go 0 graph
  where
    go :: Int -> Graph -> Maybe Int
    go minCut prunedGraph =
      case shortestPathOrConnectedGroup prunedGraph source target of
        Left path ->
          guard (minCut < 3)
            >> let withEdgesRemoved =
                     foldl' deleteEdge prunedGraph $ zip (tail path) path
                   withReverseEdgesAdded =
                     foldl' insertEdge withEdgesRemoved $ zip path (tail path)
                in go (minCut + 1) withReverseEdgesAdded
        Right groupSize -> Just groupSize
