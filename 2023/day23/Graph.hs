module Graph where

import           Data.List       (elemIndex)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromJust)
import           HikingMap       (HikingMap, allNodes, walkToNodes)

type NodeId = Int

data OutgoingPath = OutgoingPath
  { target   :: NodeId
  , distance :: Int
  } deriving (Show)

type Graph = Map NodeId [OutgoingPath]

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = fromJust $ elemIndex x xs

constructGraph :: HikingMap -> Graph
constructGraph hikingMap =
  M.fromList
    $ map
        (\i -> (i, collectOutgoings (nodeCoordinates !! i)))
        [0 .. length nodeCoordinates - 1]
  where
    nodeCoordinates = allNodes hikingMap
    collectOutgoings =
      map (\(coord, dist) -> OutgoingPath (getIndex coord nodeCoordinates) dist)
        . walkToNodes hikingMap

finalNodeId :: Graph -> Int
finalNodeId graph = M.size graph - 1

penultimateStep :: Graph -> (NodeId, Int)
penultimateStep graph = (nodeId, distance)
  where
    filterOutgoingPaths outgoings =
      case filter ((== finalNodeId graph) . target) outgoings of
        [path] -> Just path
        _      -> Nothing
    (nodeId, OutgoingPath _ distance) =
      head $ M.toList $ M.mapMaybe filterOutgoingPaths graph
