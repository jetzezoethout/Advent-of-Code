module ValveJourney where

import           Data.Bits   (Bits (..), bit, (.&.))
import           Data.IntMap ((!))
import           Data.Set    (Set)
import qualified Data.Set    as S
import           Graph       (Graph, OutgoingPath (..))

type BitMask = Int

data ValveJourney = ValveJourney
  { flowReleased    :: Int
  , currentJunction :: Int
  , minutesLeft     :: Int
  , visited         :: BitMask
  } deriving (Show, Eq, Ord)

disjoint :: ValveJourney -> ValveJourney -> Bool
disjoint j1 j2 = shiftR j1.visited 1 .&. shiftR j2.visited 1 == zeroBits

allJourneys :: Graph -> Int -> Set ValveJourney
allJourneys graph minutes = go [initial] S.empty
  where
    initial =
      ValveJourney
        { flowReleased = 0
        , currentJunction = 0
        , minutesLeft = minutes
        , visited = bit 0
        }
    go :: [ValveJourney] -> Set ValveJourney -> Set ValveJourney
    go [] found = found
    go (next:others) found =
      go (continue graph next <> others) $ S.insert next found

continue :: Graph -> ValveJourney -> [ValveJourney]
continue graph ValveJourney {..} =
  let possibleDestinations = graph ! currentJunction
      actualDestinations =
        filter
          (\OutgoingPath {..} ->
             minutesLeft > distance + 1 && not (visited `testBit` junctionIndex))
          possibleDestinations
   in map
        (\OutgoingPath {..} ->
           ValveJourney
             { flowReleased =
                 flowReleased + (minutesLeft - distance - 1) * targetFlowRate
             , currentJunction = junctionIndex
             , minutesLeft = minutesLeft - distance - 1
             , visited = visited `setBit` junctionIndex
             })
        actualDestinations

getMaxFlow :: Set ValveJourney -> Int
getMaxFlow = flowReleased . S.findMax

getMaxFlowWithElephant :: Set ValveJourney -> Int
getMaxFlowWithElephant journeys = go journeys 0
  where
    go journeysToInspect maxSoFar =
      case S.maxView journeysToInspect of
        Nothing -> maxSoFar
        Just (journey, otherJourneys) ->
          if flowReleased journey <= maxSoFar `div` 2
            then maxSoFar
            else let theBestElephant =
                       S.lookupMax $ S.filter (`disjoint` journey) otherJourneys
                     newMax =
                       case theBestElephant of
                         Nothing -> maxSoFar
                         Just elephant ->
                           max maxSoFar
                             $ flowReleased journey + flowReleased elephant
                  in go otherJourneys newMax
