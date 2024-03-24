module Main where

import           Module              (parseModules)

import           CircuitContext      (findEngineStart, initialState,
                                      pushButtonTimes)
import           Control.Monad.State (evalState)
import           ProcessFile         (processFile)
import           Signal              (Pulse (..), Signal (pulse))

main :: IO ()
main =
  processFile $ \text -> do
    let modules = parseModules text
        cleanCircuit = initialState modules
        allSignals = evalState (pushButtonTimes 1000) cleanCircuit
    print $ getAnswer1 allSignals
    print $ evalState findEngineStart cleanCircuit

getAnswer1 :: [Signal] -> Int
getAnswer1 signals =
  length (filter ((== Low) . pulse) signals)
    * length (filter ((== High) . pulse) signals)
