module Card where

import           Data.List (intersect)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Card = Card
  { cardId         :: Int
  , winningNumbers :: [Int]
  , actualNumbers  :: [Int]
  } deriving (Show)

parseNumberSequence :: Text -> [Int]
parseNumberSequence = map parseUnsignedInt . T.words

parseCardId :: Text -> Int
parseCardId text = parseUnsignedInt $ T.strip $ T.words text !! 1

parseCard :: Text -> Card
parseCard text =
  let parts = T.split (== ':') text
      numberSequences = T.split (== '|') $ parts !! 1
   in Card
        { cardId = parseCardId $ head parts
        , winningNumbers = parseNumberSequence $ head numberSequences
        , actualNumbers = parseNumberSequence $ numberSequences !! 1
        }

getCorrectNumbers :: Card -> [Int]
getCorrectNumbers Card {..} = winningNumbers `intersect` actualNumbers

getCorrectNumbersAmount :: Card -> Int
getCorrectNumbersAmount = length . getCorrectNumbers

getScore :: Card -> Int
getScore card =
  case getCorrectNumbersAmount card of
    0        -> 0
    positive -> 2 ^ (positive - 1)
