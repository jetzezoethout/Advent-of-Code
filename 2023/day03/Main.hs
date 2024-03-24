module Main where

import           Engine       (Engine, getEnginePartNumbers, getGears,
                               parseEngine)
import           EngineNumber (value)
import           Gear         (ratio)
import           ProcessFile  (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let engine = parseEngine text
    print $ getAnswer1 engine
    print $ getAnswer2 engine

getAnswer1 :: Engine -> Int
getAnswer1 = sum . map value . getEnginePartNumbers

getAnswer2 :: Engine -> Int
getAnswer2 = sum . map ratio . getGears
