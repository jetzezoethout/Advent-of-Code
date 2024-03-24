module SensorData where

import           Coordinate (Coordinate (..), manhattanDistance)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Parsers    (parseInt)

data SensorData = SensorData
  { sensor        :: Coordinate
  , closestBeacon :: Coordinate
  , radius        :: Int
  } deriving (Show)

parseSensorData :: Text -> SensorData
parseSensorData text =
  SensorData
    { sensor = sensor
    , closestBeacon = closestBeacon
    , radius = manhattanDistance sensor closestBeacon
    }
  where
    parts = T.words text
    parseRHS definition = parseInt $ T.splitOn "=" definition !! 1
    sensor = Coordinate (parseRHS $ parts !! 3) (parseRHS $ parts !! 2)
    closestBeacon = Coordinate (parseRHS $ parts !! 9) (parseRHS $ parts !! 8)

isOutside :: Coordinate -> SensorData -> Bool
coordinate `isOutside` sensorData =
  manhattanDistance coordinate sensorData.sensor > radius sensorData

isInsideRange :: Coordinate -> Bool
isInsideRange Coordinate {..} =
  0 <= row && row <= 4000000 && 0 <= column && column <= 4000000

isPossibleBeacon :: [SensorData] -> Coordinate -> Bool
isPossibleBeacon sensorDatas coordinate =
  isInsideRange coordinate && all (coordinate `isOutside`) sensorDatas
