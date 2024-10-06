module Cups where

import           Control.Monad               ((>=>))
import           Control.Monad.ST            (ST, runST)
import           Data.Char                   (digitToInt, intToDigit)
import           Data.Foldable               (traverse_)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as V

type Cup = Int

previousCup :: Cup -> Cup -> Cup
previousCup maxCup cup = ((cup - 2) `mod` maxCup) + 1

parseCupList :: Text -> [Int]
parseCupList = map digitToInt . T.unpack

data Cups s = Cups
  { currentCup :: Cup
  , getNextCup :: MVector s Cup
  }

initializeCups :: [Int] -> ST s (Cups s)
initializeCups cupList = do
  getNextCup <- V.new 10
  traverse_ (uncurry $ V.write getNextCup) $ zip cupList $ tail $ cycle cupList
  return $ Cups {currentCup = head cupList, getNextCup = getNextCup}

pickUp :: Cups s -> ST s ([Cup], Cups s)
pickUp Cups {..} = do
  first <- V.read getNextCup currentCup
  second <- V.read getNextCup first
  third <- V.read getNextCup second
  afterPickedUp <- V.read getNextCup third
  V.write getNextCup currentCup afterPickedUp
  return
    ( [first, second, third]
    , Cups {currentCup = currentCup, getNextCup = getNextCup})

findDestination :: Cup -> Cup -> [Cup] -> Cup
findDestination maxCup currentCup pickedUp = go $ previousCup maxCup currentCup
  where
    go candidate =
      if candidate `elem` pickedUp
        then go (previousCup maxCup candidate)
        else candidate

playOnce :: Cups s -> ST s (Cups s)
playOnce cups = do
  (pickedUp, Cups {..}) <- pickUp cups
  let maxCup = V.length cups.getNextCup - 1
      destination = findDestination maxCup currentCup pickedUp
  nextToDestination <- V.read getNextCup destination
  V.write getNextCup destination $ head pickedUp
  V.write getNextCup (last pickedUp) nextToDestination
  nextCurrentCup <- V.read getNextCup currentCup
  return $ Cups {currentCup = nextCurrentCup, getNextCup = getNextCup}

play :: Int -> Cups s -> ST s (Cups s)
play n = foldr (>=>) return $ replicate n playOnce

representation :: Cups s -> ST s String
representation Cups {..} = go 1
  where
    go cup = do
      nextCup <- V.read getNextCup cup
      if nextCup == 1
        then return ""
        else do
          remaining <- go nextCup
          return $ intToDigit nextCup : remaining

fullGame :: [Cup] -> String
fullGame cupList = runST go
  where
    go = initializeCups cupList >>= play 100 >>= representation

initializeManyCups :: [Cup] -> ST s (Cups s)
initializeManyCups cupList = do
  getNextCup <- V.generate 1000001 (+ 1)
  traverse_ (uncurry $ V.write getNextCup) $ zip cupList $ tail cupList
  V.write getNextCup (last cupList) 10
  V.write getNextCup 1000000 $ head cupList
  return $ Cups {currentCup = head cupList, getNextCup = getNextCup}

representationForManyCups :: Cups s -> ST s Int
representationForManyCups Cups {..} = do
  first <- V.read getNextCup 1
  second <- V.read getNextCup first
  return $ first * second

fullGameWithManyCups :: [Cup] -> Int
fullGameWithManyCups cupList = runST go
  where
    go =
      initializeManyCups cupList >>= play 10000000 >>= representationForManyCups
