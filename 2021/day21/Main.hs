module Main where

import           Game        (parseGame, playDeterminstic, playDirac)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let game = parseGame text
    print $ playDeterminstic game
    print $ playDirac game
