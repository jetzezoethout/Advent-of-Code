module Race where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Parsers   (parseUnsignedInt)

data Race = Race
  { time   :: Int
  , toBeat :: Int
  } deriving (Show)

parseRaces :: Text -> Text -> [Race]
parseRaces timeLine distanceLine =
  zipWith Race (parseNumbers timeLine) (parseNumbers distanceLine)
  where
    parseNumbers :: Text -> [Int]
    parseNumbers text =
      map parseUnsignedInt $ T.words $ T.split (== ':') text !! 1

parseSingleRace :: Text -> Text -> Race
parseSingleRace timeLine distanceLine =
  Race (parseBigNumber timeLine) (parseBigNumber distanceLine)
  where
    parseBigNumber :: Text -> Int
    parseBigNumber text =
      parseUnsignedInt $ T.filter (not . isSpace) $ T.split (== ':') text !! 1

minimumWinTime :: Race -> Int
minimumWinTime Race {..} = floor xMin + 1
  where
    toDouble :: Int -> Double
    toDouble = fromIntegral
    discriminant = time * time - 4 * toBeat
    xMin = 0.5 * (toDouble time - sqrt (toDouble discriminant))

maximumWinTime :: Race -> Int
maximumWinTime race = time race - minimumWinTime race

numberOfWinTimes :: Race -> Int
numberOfWinTimes race = maximumWinTime race - minimumWinTime race + 1
