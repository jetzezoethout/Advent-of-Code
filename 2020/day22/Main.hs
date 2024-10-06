module Main where

import           Deck        (score)
import           Game        (Result (winningDeck), parseGame, playNormal,
                              playRecursive)
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let game = parseGame text
    print $ score $ winningDeck $ playNormal game
    print $ score $ winningDeck $ playRecursive game
