module Part1
  ( getAnswer
  ) where

import           Data.Char (isAlpha)
import           Data.Text (Text)
import qualified Data.Text as T

trimText :: Text -> Text
trimText = T.dropAround isAlpha

trimmedTextToCalibration :: Text -> Int
trimmedTextToCalibration text = read @Int $ [T.head text, T.last text]

getCalibration :: Text -> Int
getCalibration = trimmedTextToCalibration . trimText

totalScore :: [Text] -> Int
totalScore = sum . map getCalibration

getAnswer :: Text -> Int
getAnswer = totalScore . T.lines
