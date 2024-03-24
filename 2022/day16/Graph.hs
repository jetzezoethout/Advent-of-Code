module Graph where

import           Control.Monad.State (State, evalState, gets, modify)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as M
import           Data.List           (elemIndex)
import           Data.Map            ((!))
import           Data.Sequence       (Seq, ViewL (EmptyL, (:<)), fromList,
                                      singleton, viewl, (><))
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import           ValveInfo           (ValveInfo (..), ValveInfos, getJunctions)

data OutgoingPath = OutgoingPath
  { junctionIndex  :: Int
  , targetFlowRate :: Int
  , distance       :: Int
  } deriving (Show)

type Graph = IntMap [OutgoingPath]

constructGraph :: ValveInfos -> Graph
constructGraph valves = M.fromList $ map getPair [0 .. length junctions - 1]
  where
    junctions = getJunctions valves
    getPair index = (index, findShortestPaths valves $ junctions !! index)

type VisitedNodes = Set Text

data SearchStage = SearchStage
  { currentJunction   :: Text
  , distanceTravelled :: Int
  }

findShortestPaths :: ValveInfos -> Text -> [OutgoingPath]
findShortestPaths valves start =
  evalState (go $ singleton (SearchStage start 0)) $ S.singleton start
  where
    junctions = getJunctions valves
    go :: Seq SearchStage -> State VisitedNodes [OutgoingPath]
    go livePaths =
      case viewl livePaths of
        EmptyL -> return []
        SearchStage {..} :< remainingStages -> do
          let addNewPath :: [OutgoingPath] -> [OutgoingPath]
              addNewPath =
                case currentJunction `elemIndex` junctions of
                  Just index ->
                    (OutgoingPath
                       { junctionIndex = index
                       , targetFlowRate = flowRate (valves ! currentJunction)
                       , distance = distanceTravelled
                       } :)
                  Nothing -> id
          targets <-
            gets $ \visited ->
              filter
                (`S.notMember` visited)
                (tunnelsTo $ valves ! currentJunction)
          mapM_ (modify . S.insert) targets
          addNewPath
            <$> go
                  (remainingStages
                     >< fromList
                          (map
                             (\t -> SearchStage t (distanceTravelled + 1))
                             targets))
