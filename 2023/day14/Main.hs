module Main where

import           Control.Monad.Reader (ReaderT (runReaderT))
import           Control.Monad.State  (evalState)
import           Direction            (Direction (North))
import           PlatformContext      (resultOfCycling, rollTowards, score)
import           ProcessFile          (processFile)
import           RockContainers       (parseFixedRocks, parseRollingRocks)

main :: IO ()
main =
  processFile $ \text -> do
    let rollingRocks = parseRollingRocks text
        fixedRocks = parseFixedRocks text
        scoreAfterTiltingNorth =
          evalState
            (runReaderT (rollTowards North >> score) fixedRocks)
            rollingRocks
        stateAfterABillionCycles =
          evalState
            (runReaderT (resultOfCycling 1000000000) fixedRocks)
            rollingRocks
    print scoreAfterTiltingNorth
    print $ evalState (runReaderT score fixedRocks) stateAfterABillionCycles
