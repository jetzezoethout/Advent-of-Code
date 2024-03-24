module Main where

import           Cave        (parseCave)
import qualified Data.Set    as S
import           ProcessFile (processFile)
import           Sand        (fallUntilIOriginBlocked, fallUntilSandDisappears)

main :: IO ()
main =
  processFile $ \text -> do
    let cave = parseCave text
        finalCave = fallUntilSandDisappears cave
        finalCaveWithFloor = fallUntilIOriginBlocked cave
    print $ S.size finalCave - S.size cave
    print $ S.size finalCaveWithFloor - S.size cave
