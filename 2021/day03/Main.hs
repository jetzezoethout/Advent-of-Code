module Main where

import           Binary      (getCO2Rating, getGammaRate, getOxygenRating,
                              invert, parseBinary, toInt)
import qualified Data.Text   as T
import           ProcessFile (processFile)

main :: IO ()
main =
  processFile $ \text -> do
    let report = map parseBinary $ T.lines text
        gammaRate = getGammaRate report
        epsilonRate = invert gammaRate
        oxygenRating = getOxygenRating report
        co2Rating = getCO2Rating report
    print $ toInt gammaRate * toInt epsilonRate
    print $ toInt oxygenRating * toInt co2Rating
