module Part2
  ( getAnswer
  ) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           TextToNumber (extractNumber)

extractNumbers :: Text -> [Int]
extractNumbers text =
  case fmap snd (T.uncons text) of
    Nothing -> []
    Just tailText ->
      let remainder = extractNumbers tailText
       in case extractNumber text of
            Nothing    -> remainder
            Just value -> value : remainder

numbersToCalibration :: Num a => [a] -> a
numbersToCalibration numbers = 10 * head numbers + last numbers

getCalibration :: Text -> Int
getCalibration = numbersToCalibration . extractNumbers

totalScore :: [Text] -> Int
totalScore = sum . map getCalibration

getAnswer :: Text -> Int
getAnswer = totalScore . T.lines
