module TextToNumber
  ( extractNumber
  ) where

import           Control.Monad (guard)
import           Data.Monoid   (First (First, getFirst))
import           Data.Text     (Text)
import qualified Data.Text     as T

data TextToNumber = TextToNumber
  { prefix :: Text
  , value  :: Int
  } deriving (Show)

getValue :: Text -> TextToNumber -> Maybe Int
getValue text TextToNumber {..} = guard (T.isPrefixOf prefix text) >> Just value

getFirstValue :: Text -> [TextToNumber] -> Maybe Int
getFirstValue text = getFirst . mconcat . map (First . getValue text)

extractNumber :: Text -> Maybe Int
extractNumber text = getFirstValue text allRecords

numbersAsWords :: [Text]
numbersAsWords =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

alphaRecords :: [TextToNumber]
alphaRecords = map (\i -> TextToNumber (numbersAsWords !! (i - 1)) i) [1 .. 9]

numRecords :: [TextToNumber]
numRecords = map (\i -> TextToNumber (T.pack $ show @Int i) i) [1 .. 9]

allRecords :: [TextToNumber]
allRecords = numRecords <> alphaRecords
