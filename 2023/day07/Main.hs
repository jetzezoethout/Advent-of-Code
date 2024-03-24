module Main where

import           Bid         (Bid, parseBid, rankedScore)
import           Data.List   (sort, sortBy)
import qualified Data.Text   as T
import           JokerOrd    (JokerOrd (jokerCompare))
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let bids = map parseBid $ T.lines text
    print $ getAnswer1 bids
    print $ getAnswer2 bids

getAnswer1 :: [Bid] -> Int
getAnswer1 = sum . zipWith rankedScore [1 ..] . sort

getAnswer2 :: [Bid] -> Int
getAnswer2 = sum . zipWith rankedScore [1 ..] . sortBy jokerCompare
