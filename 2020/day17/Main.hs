module Main where

import           ConwayCubes (amountActive, cycleTimes, parseConwayCubes3D,
                              parseConwayCubes4D)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let conwayCubes3D = parseConwayCubes3D text
        conwayCubes4D = parseConwayCubes4D text
    print $ amountActive $ cycleTimes 6 conwayCubes3D
    print $ amountActive $ cycleTimes 6 conwayCubes4D
