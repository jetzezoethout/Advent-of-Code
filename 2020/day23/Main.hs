module Main where

import           Cups        (fullGame, fullGameWithManyCups, parseCupList)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let cupList = parseCupList text
    putStrLn $ fullGame cupList
    print $ fullGameWithManyCups cupList
