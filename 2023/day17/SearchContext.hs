module SearchContext where

import           City                 (City, allNodesAsc, isFinalNode)
import           Control.Monad.Reader (MonadReader (reader),
                                       ReaderT (runReaderT))
import           Control.Monad.State  (MonadState (put), State, evalState, gets,
                                       modify)
import           Coordinate           (Coordinate (Coordinate))
import           Data.PSQueue         (Binding ((:->)), PSQ)
import qualified Data.PSQueue         as Q
import           Node                 (Node (Node), Orientation (..))
import           OutgoingPath         (OutgoingPath (..))

data Tag
  = Distance Int
  | NotSeen
  deriving (Show, Eq, Ord)

updateTag :: Int -> Tag -> Tag
updateTag tentativeDistance (Distance distance) =
  Distance $ min tentativeDistance distance
updateTag tentativeDistance NotSeen = Distance tentativeDistance

type SearchData = PSQ Node Tag

type SearchContext = ReaderT City (State SearchData)

initialize :: SearchContext ()
initialize = do
  allNodes <- reader allNodesAsc
  put $ Q.fromAscList $ map (:-> NotSeen) allNodes
  mapM_
    (modify . (`Q.insert` Distance 0))
    [Node (Coordinate 0 0) Horizontal, Node (Coordinate 0 0) Vertical]

findShortestPath ::
     (Node -> City -> [OutgoingPath]) -> SearchContext (Maybe Int)
findShortestPath pathGetter = initialize >> go
  where
    go = do
      nextStep <- gets Q.minView
      case nextStep of
        Nothing -> return Nothing
        Just (binding, newQueue) ->
          let nextNode = Q.key binding
              tag = Q.prio binding
           in case tag of
                NotSeen -> return Nothing
                Distance distance -> do
                  done <- reader $ isFinalNode nextNode
                  if done
                    then return $ Just distance
                    else do
                      put newQueue
                      pathsToTake <- reader $ pathGetter nextNode
                      mapM_
                        (\OutgoingPath {..} ->
                           modify
                             $ Q.adjust (updateTag (distance + cost)) target)
                        pathsToTake
                      go

runSearchContext :: City -> SearchContext a -> a
runSearchContext city context = evalState (runReaderT context city) Q.empty
