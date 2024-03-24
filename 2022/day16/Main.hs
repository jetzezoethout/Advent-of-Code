module Main where

import qualified Data.Set     as S
import           Graph        (constructGraph)
import           ProcessFile  (processFile)
import           ValveInfo    (parseValves)
import           ValveJourney (ValveJourney (..), allJourneys,
                               getMaxFlowWithElephant)

main :: IO ()
main =
  processFile $ \text -> do
    let valves = parseValves text
        graph = constructGraph valves
    print $ flowReleased $ S.findMax $ allJourneys graph 30
    print $ getMaxFlowWithElephant $ allJourneys graph 26
