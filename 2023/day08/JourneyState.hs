module JourneyState where

import           DesertMap (DesertMap, findJunction)
import           Direction (Direction)
import           Junction  (Node, takeJunction)

data JourneyState = JourneyState
  { currentNode :: Node
  , stepsTaken  :: Int
  } deriving (Show)

goOnAdventure :: [Direction] -> DesertMap -> (Node -> Bool) -> Node -> Int
goOnAdventure directions desertMap shouldStop startNode =
  go directions $ JourneyState startNode 0
  where
    go :: [Direction] -> JourneyState -> Int
    go directionsToGo JourneyState {..} =
      let nextJunction = findJunction desertMap currentNode
          nextNode = takeJunction (head directionsToGo) nextJunction
          nextState =
            JourneyState {currentNode = nextNode, stepsTaken = stepsTaken + 1}
       in if shouldStop nextState.currentNode
            then nextState.stepsTaken
            else go (tail directionsToGo) nextState
