module Main where

import           Card        (Card, getScore, parseCard)
import           ClonedCard  (ClonedCard (copies), cascadeClones, single)
import qualified Data.Text   as T
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let cards = map parseCard $ T.lines text
    print $ getAnswer1 cards
    print $ getAnswer2 cards

getAnswer1 :: [Card] -> Int
getAnswer1 = sum . map getScore

getAnswer2 :: [Card] -> Int
getAnswer2 = sum . map copies . cascadeClones . map single
