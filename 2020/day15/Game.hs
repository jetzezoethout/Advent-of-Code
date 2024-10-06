module Game where

import           Control.Monad.ST            (ST, runST)
import           Data.Foldable               (traverse_)
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

data Game s = Game
  { turn             :: Int
  , currentlySpoken  :: Int
  , previouslySpoken :: MVector s Int
  }

initializeGame :: Int -> [Int] -> ST s (Game s)
initializeGame rounds initialNumbers = do
  previouslySpoken <- initialize
  return
    $ Game
        { turn = length initialNumbers
        , currentlySpoken = last initialNumbers
        , previouslySpoken = previouslySpoken
        }
  where
    initialize :: ST s (MVector s Int)
    initialize = do
      previouslySpoken <- V.replicate rounds 0
      traverse_ (\(turn, spoken) -> V.write previouslySpoken spoken turn)
        $ zip [1 ..]
        $ init initialNumbers
      return previouslySpoken

doTurn :: Game s -> ST s (Game s)
doTurn Game {..} = do
  previouslySpokenInTurn <- V.read previouslySpoken currentlySpoken
  let toSpeak =
        if previouslySpokenInTurn == 0
          then 0
          else turn - previouslySpokenInTurn
  V.write previouslySpoken currentlySpoken turn
  return
    Game
      { turn = turn + 1
      , currentlySpoken = toSpeak
      , previouslySpoken = previouslySpoken
      }

playRounds :: Int -> [Int] -> Int
playRounds rounds initialNumbers =
  runST $ initializeGame rounds initialNumbers >>= go
  where
    go :: Game s -> ST s Int
    go game =
      if turn game == rounds
        then return $ currentlySpoken game
        else doTurn game >>= go
