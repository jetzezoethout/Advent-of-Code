module SearchContext where

import           City                 (City, isFinalNode)
import           Control.Monad.Reader (MonadReader (reader),
                                       ReaderT (runReaderT))
import           Control.Monad.State  (MonadState (put), State, evalState, gets,
                                       modify)
import           Coordinate           (Coordinate (Coordinate))
import           Data.PSQueue         (PSQ)
import qualified Data.PSQueue         as Q
import           Node                 (Node (Node), Orientation (..))
import           OutgoingPath         (OutgoingPath (..))

data Tag
  = Distance Int
  | Seen
  deriving (Eq, Ord, Show)

updateTag :: Int -> Maybe Tag -> Tag
updateTag tentativeDistance oldTag =
  case oldTag of
    Nothing                  -> Distance tentativeDistance
    Just Seen                -> Seen
    Just (Distance distance) -> Distance $ min distance tentativeDistance

type SearchData = PSQ Node Tag

type SearchContext = ReaderT City (State SearchData)

initialize :: SearchContext ()
initialize =
  mapM_
    (modify . flip Q.insert (Distance 0))
    [Node (Coordinate 0 0) Horizontal, Node (Coordinate 0 0) Vertical]

findShortestPath ::
     (Node -> City -> [OutgoingPath]) -> SearchContext (Maybe Int)
findShortestPath pathGetter = initialize >> go
  where
    go = do
      nextStep <- gets Q.minView
      case nextStep of
        Nothing -> return Nothing
        Just (binding, newQueue) -> do
          let nextNode = Q.key binding
              tag = Q.prio binding
          put newQueue
          modify $ Q.insert nextNode Seen
          case tag of
            Seen -> return Nothing
            Distance distance -> do
              done <- reader $ isFinalNode nextNode
              if done
                then return $ Just distance
                else do
                  pathsToTake <- reader $ pathGetter nextNode
                  mapM_
                    (\OutgoingPath {..} ->
                       modify
                         $ Q.alter (Just . updateTag (distance + cost)) target)
                    pathsToTake
                  go

runSearchContext :: City -> SearchContext a -> a
runSearchContext city context = evalState (runReaderT context city) Q.empty
